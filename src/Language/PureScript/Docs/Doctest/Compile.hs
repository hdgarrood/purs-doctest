{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Docs.Doctest.Compile
  ( entryPointModule
  , examplesModule
  ) where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Arrow (second, (&&&), (***))
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Either (partitionEithers)
import qualified Data.Text as Text
import qualified Cheapskate

import qualified Language.PureScript as P
import qualified Language.PureScript.PSString as P
import qualified Language.PureScript.Interactive as Interactive

import Language.PureScript.Docs.Doctest.Types (Example(..), Examples(..))

-- | Given a set of examples, create an entry point module which collects them
-- all into a single value, ready for processing with a doctest runner.
--
-- The resulting module exports one value, `main`, whose type is
--
--
--  Array
--    { moduleName :: String
--    , examples :: Array
--      { title :: String
--      , examples :: Array
--          { actual :: String
--          , expected :: String
--          }
--      }
--    }
--
entryPointModule :: [Examples] -> P.Module
entryPointModule = go . map (mkDoctestModuleName . examplesModuleName)
  where
  go modNames =
    let
      moduleName =
        P.moduleNameFromString "$Doctest.$Main"
      doctestImport mn =
        (mn, P.Implicit, Just mn)
      main =
        mainDecl modNames
    in
      P.Module
        internalSpan
        []
        moduleName
        (map (importDecl . doctestImport) modNames ++ [main])
        Nothing

mainDecl :: [P.ModuleName] -> P.Declaration
mainDecl modNames =
  basicValueDecl "main"
    (P.Literal
      (P.ArrayLiteral
        (map examplesFor modNames)))
  where
  examplesFor mn =
    P.Literal (P.ObjectLiteral
      [ ( P.mkString "moduleName"
        , P.Literal (P.StringLiteral (P.mkString (P.runModuleName mn)))
        )
      , ( P.mkString "examples"
        , P.Var (P.Qualified (Just mn) (P.Ident "examples"))
        )
      ])


-- | Create a value declaration given a name and a RHS.
basicValueDecl :: Text -> P.Expr -> P.Declaration
basicValueDecl ident expr =
  P.ValueDecl
    (internalSpan, [])
    (P.Ident ident)
    P.Public
    []
    [P.MkUnguarded expr]

-- | Create a module for running doctest examples. The resulting module will
-- export a single value, `examples`, whose type is
--
--   Array { title :: String
--         , examples :: Array { actual :: String, expected :: String }
--         }
examplesModule :: Examples -> P.Module
examplesModule egs =
  let
    moduleName =
      mkDoctestModuleName (examplesModuleName egs)
    -- We need this to be able to `show` the results of the examples, in order
    -- to compare them to the expected output.
    preludeImport =
      ( P.moduleNameFromString "Prelude"
      , P.Implicit
      , Just magicPrelude
      )
    moduleImport =
      ( examplesModuleName egs
      , P.Implicit
      , Nothing
      )
    examplesDecl =
      basicValueDecl "examples" examplesValue
    examplesValue =
      P.Literal (P.ArrayLiteral
        (fmap (uncurry toCases) (examplesExamples egs)))
  in
    P.Module
      internalSpan
      []
      moduleName
      (map importDecl (preludeImport : moduleImport : examplesImports egs)
       ++ [examplesDecl])
      Nothing

toCases :: Text -> NonEmpty Example -> P.Expr
toCases title egs =
  P.Literal (P.ObjectLiteral
    [ (P.mkString "title", P.Literal (P.StringLiteral (P.mkString title)))
    , (P.mkString "examples", examplesToExpr egs)
    ])

mkDoctestModuleName :: P.ModuleName -> P.ModuleName
mkDoctestModuleName (P.ModuleName parts) =
  P.ModuleName (P.ProperName "$Doctest" : parts)

-- | Convert a set of examples corresponding to a single declaration into an
-- Expr, e.g. the following
--
-- >>> x = 2 + 3
-- >>> y = 3 + x
-- >>> y
-- 8
-- >>> y * 2
-- 16
--
-- would be converted to
--
-- let
--  x = 2 + 3
--  y = 3 + x
-- in
--  [ { actual: show y, expected: "8" }
--  , { actual: show (y * 2), expected: "16" }
--  ]
--
-- The resulting Expr assumes that Prelude has been imported as "$Prelude" in
-- order to be able to safely reference `show`.
--
-- The type of the resulting Expr is always
--
--   Array { expected :: String, actual :: String }
--
examplesToExpr :: NonEmpty Example -> P.Expr
examplesToExpr egs =
  if null assigns'
    then cases'
    else P.Let assigns' cases'
  where
  assigns' =
    map P.ValueDeclaration assigns

  cases' =
    P.Literal (P.ArrayLiteral (fmap toCase evals))

  toCase (expr, out) =
    P.Literal (P.ObjectLiteral
      [ (P.mkString "actual", P.App showVar expr)
      , (P.mkString "expected", P.Literal (P.StringLiteral (P.mkString out)))
      ])

  showVar =
    P.Var (P.Qualified (Just magicPrelude) (P.Ident "show"))

  (assigns, evals) =
    partitionEithers (NonEmpty.toList (fmap toEither egs))

  toEither (Assign assign) = Left assign
  toEither (Evaluate expr out) = Right (expr, out)

-- | A name for Prelude to be imported as, with an invalid name so as to make
-- sure clashes can't happen.
magicPrelude :: P.ModuleName
magicPrelude = P.moduleNameFromString "$Prelude"

internalSpan :: P.SourceSpan
internalSpan = P.internalModuleSourceSpan "<internal>"

importDecl :: Interactive.ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) =
  P.ImportDeclaration (internalSpan, []) mn declType asQ
