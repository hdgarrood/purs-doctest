{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty (NonEmpty)
import Control.Category ((>>>))
import Control.Arrow (second)
import Control.Monad.Trans.Except (runExceptT)
import System.FilePath.Glob (glob)
import System.Exit (exitFailure)
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, parsePackageName)

import Language.PureScript.Docs.Doctest

dummy :: PackageName
Right dummy = parsePackageName "dummy"

main :: IO ()
main = do
  egs <- collectExamples
  putStrLn (show egs)

collectExamples :: IO [Examples]
collectExamples = do
  input <- glob "src/**/*.purs"
  deps <- map (dummy,) <$> glob ".psc-package/psc-0.11.7/*/*/src/**/*.purs"

  modules <- parseAndConvert input deps
  pure (map parseDoctests modules)

  where
  successOrExit :: Either P.MultipleErrors a -> IO a
  successOrExit act =
    case act of
      Right x ->
        return x
      Left err -> do
        putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions err)
        exitFailure

  parseAndConvert input deps =
    runExceptT (D.parseFilesInPackages input deps
               >>= uncurry D.convertModulesInPackage)
    >>= successOrExit
