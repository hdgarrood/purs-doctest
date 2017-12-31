{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.PureScript.Docs.Doctest where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Foldable (toList)
import qualified Cheapskate

import qualified Language.PureScript.Docs as Docs

data Example = Example
  { exampleInput :: Text
  , exampleExpectedOutput :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | The string which marks the start of a doctest example.
doctestMarker :: Text
doctestMarker = ">>> "

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
parseDoctests :: Text -> [Example]
parseDoctests = go . Cheapskate.markdown Cheapskate.def
  where
  go :: Cheapskate.Doc -> [Example]
  go (Cheapskate.Doc _ blocks) = concatMap goBlock (toList blocks)

  goBlock :: Cheapskate.Block -> [Example]
  goBlock (Cheapskate.CodeBlock (Cheapskate.CodeAttr "purescript" _) text) =
    parseCodeBlock text
  goBlock _ =
    []

-- | Parse the contents of a code block.
parseCodeBlock :: Text -> [Example]
parseCodeBlock = unfoldr extractExample . Text.lines

-- |
-- Given some lines, attempt to parse a single example from a pair of two
-- lines, starting from lines 1 and 2, and then trying lines 2 and 3, then 3
-- and 4, and so on. This function returns Just an example and the remainder of
-- the lines after parsing it, or Nothing if no examples can be parsed.
--
extractExample :: [Text] -> Maybe (Example, [Text])
extractExample (x:y:xs) =
  fmap rest (parseExample x (Just y))
  <|> extractExample (y:xs)
  where
  rest e =
    (e, rest' e)
  rest' e =
    if isJust (exampleExpectedOutput e)
      then xs
      else y:xs
extractExample [x] =
  fmap (,[]) (parseExample x Nothing)
extractExample _ =
  Nothing

-- |
-- Given two consecutive lines of text (or just one if we are at the end of the
-- comment), attempt to parse an Example.
--
parseExample :: Text -> Maybe Text -> Maybe Example
parseExample line1 mline2 = do
  input <- Text.stripPrefix doctestMarker line1
  pure (Example input output)
  where
  output = do
    l <- mline2
    guard (not (doctestMarker `Text.isPrefixOf` l))
    pure l
