{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Language.PureScript.Docs.Doctest.Types
  ( Example(..)
  , Examples(..)
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
import qualified Language.PureScript.Docs as Docs
import qualified Language.PureScript.Interactive as Interactive

-- todo: include lines in source files in errors.
-- todo: allow Eff actions in doctests
-- todo: Disambiguate when e.g. there is a data constructor and a type who
-- share a name, or a type and a kind.

data Example
  -- | An assignment such as `x = 4 * 5`.
  = Assign (P.ValueDeclarationData [P.GuardedExpr])
  -- | Evaluates an expression like, say `x + 3`, calls `show` on it, and
  -- compares the result with an expected result (as a Text) like `23`.
  | Evaluate P.Expr Text
  deriving (Show)

-- | A set of examples from a given module.
data Examples = Examples
  { examplesModuleName :: P.ModuleName
  -- | A set of imports to be used for all examples.
  , examplesImports :: [Interactive.ImportedModule]
  -- | Successfully parsed examples.
  , examplesExamples :: [(Text, NonEmpty Example)]
  }
  deriving (Show)
