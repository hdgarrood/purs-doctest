{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Language.PureScript.Docs.Doctest
  ( Example(..)
  , parseDoctestsFromModules
  , parseDoctests
  , parseFromDeclaration
  , parseComment
  , parseCodeBlock
  ) where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Arrow (second, (&&&))
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Maybe (isJust, listToMaybe)
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

-- | The string which marks the start of a doctest example.
doctestMarker :: Text
doctestMarker = ">>> "

parseDoctestsFromModules ::
  [Docs.Module] ->
  ( [(P.ModuleName, [(Text, [String])])]
  , [(P.ModuleName, [(Text, [Example])])]
  )
parseDoctestsFromModules =
  unzipAssoc . map (Docs.modName &&& parseDoctests)

-- |
-- Extract all examples from a module.
--
-- Note that we do not include examples from re-exports, as those may require
-- different imports, and also should not cause tests to fail (if they come
-- from different packages).
--
parseDoctests ::
  Docs.Module ->
  ( [(Text, [String])]
  , [(Text, [Example])]
  )
parseDoctests =
  foldMap parseFromDeclaration . Docs.modDeclarations

parseFromDeclaration ::
  Docs.Declaration ->
  ( [(Text, [String])]
  , [(Text, [Example])]
  )
parseFromDeclaration decl =
  unzipAssoc $
    go Docs.declTitle Docs.declComments decl
    : map
        (go Docs.cdeclTitle Docs.cdeclComments)
        (Docs.declChildren decl)
  where
  go title coms =
    title &&& (maybe mempty parseComment . coms)

unzipAssoc :: [(a, (b,c))] -> ( [(a, b)], [(a, c)] )
unzipAssoc xs =
  ( map (second fst) xs, map (second snd) xs )

-- |
-- Given a comment, attempt to parse some doctest examples out of it. Doctest
-- examples are expected to use the following format:
--
-- ```purescript
-- >>> input expression
-- expected output
-- >>> another input expression
-- expected output
-- ```
--
-- Note that it must be a fenced code block with the "purescript" language
-- attribute.
--
-- If a line starts with `>>>`, it is interpreted as a doctest input, and the
-- entire line following it is interpreted as the expected output (unless that
-- line starts with >>> too, in which case that line is interpreted as the next
-- example, and the previous example's output is not checked). All other text
-- is ignored.
--
parseComment :: Text -> ([String], [Example])
parseComment = go . Cheapskate.markdown Cheapskate.def
  where
  go :: Cheapskate.Doc -> ([String], [Example])
  go (Cheapskate.Doc _ blocks) = foldMap goBlock blocks

  goBlock :: Cheapskate.Block -> ([String], [Example])
  goBlock (Cheapskate.CodeBlock (Cheapskate.CodeAttr "purescript" _) text) =
    parseCodeBlock text
  goBlock _ =
    mempty

-- | Parse the contents of a code block.
parseCodeBlock :: Text -> ([String], [Example])
parseCodeBlock =
  partitionEithers . unfoldr extractExample . Text.lines

-- |
-- Given some lines, attempt to parse a single example from a pair of two
-- lines, starting from lines 1 and 2, and then trying lines 2 and 3, then 3
-- and 4, and so on. This function returns Just an example and the remainder of
-- the lines after parsing it, or Nothing if no examples can be parsed.
--
extractExample :: [Text] -> Maybe (Either String Example, [Text])
extractExample (x:xs) =
  fmap rest (parseExample x (listToMaybe xs))
  <|> extractExample xs
  where
  rest e =
    (e, rest' e)

  rest' (Right Evaluate{}) = drop 1 xs
  rest' (Right Assign{}) = xs
  rest' (Left _) = xs
extractExample [] =
  Nothing

-- |
-- Given two consecutive lines of text (or just one if we are at the end of the
-- comment), attempt to parse an Example.
--
-- If the first line does not start with ">>> ", which marks the start of an
-- example, this function returns Nothing. Otherwise, this function returns
-- either a String error message, saying why the example could not be parsed,
-- or an Example.
--
parseExample :: Text -> Maybe Text -> Maybe (Either String Example)
parseExample line1 mline2 = do
  input <- Text.stripPrefix doctestMarker line1
  pure (Interactive.parseCommand (Text.unpack input) >>= mkExample)
  where
  output = do
    l <- mline2
    guard (not (doctestMarker `Text.isPrefixOf` l))
    pure l

  mkExample :: Interactive.Command -> Either String Example
  mkExample cmd = case (cmd, output) of
    (Interactive.Expression expr, Just out) ->
      Right (Evaluate expr out)
    (Interactive.Expression _, Nothing) ->
      Left "Need an expected output for this example"
    (Interactive.Decls [P.ValueDeclaration declData], _) ->
      Right (Assign declData)
    _ ->
      Left "This kind of declaration is not supported in doctests."
