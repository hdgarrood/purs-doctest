-- | An example of doctests.
-- |
-- | The examples in this module use the following imports:
-- | ```purescript imports
-- | import Data.List (List(..), (:))
-- | ```
module Example where

import Prelude
import Data.List (List(..), (:))
import Data.List as List

-- | Given two sorted lists, interleave them to produce a new sorted list
-- | containing all elements from both lists.
-- |
-- | ```purescript
-- | >>> interleave (1:3:5:Nil) (2:4:6:Nil)
-- | (1:2:3:4:5:6:Nil)
-- |
-- | >>> interleave (1:2:3:Nil) (4:5:6:Nil)
-- | (1:2:3:4:5:6:Nil)
interleave :: forall a. Ord a => List a -> List a -> List a
interleave xs Nil = xs
interleave Nil xs = xs
interleave (x:xs) (y:ys) =
  if x < y
    then x : interleave xs (y:ys)
    else y : interleave (x:xs) ys
