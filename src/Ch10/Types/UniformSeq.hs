module Ch10.Types.UniformSeq where

import BasicPrelude

-- https://h2.jaguarpaw.co.uk/posts/polymorphic-recursion-combinator/
-- ^Nice section on polymorphic recursion in Haskell (and a polymorphic fix)

---------------------------------------------------------------------------------------------------
-- Non-Uniformly Recursive Sequences

-- Isomorphic to `Cofree Pair`
data NUSeq a = NUNil | NUCons a (NUSeq (a, a))

-- |O(n)
sizeL :: [a] -> Int
sizeL []     = 0
sizeL (_:xs) = 1 + sizeL xs

-- |O(log n)
sizeS :: NUSeq a -> Int
sizeS NUNil         = 0
sizeS (NUCons _ xs) = 1 + 2 * sizeS xs

---------------------------------------------------------------------------------------------------
-- Uniformly Recursive Sequences

-- |Elements and Pairs (of Elements or Pairs)
-- Isomorphic to `Free Pair` / Binary Leaf Trees
data ElemOrPair a = Elem a | Pair (ElemOrPair a) (ElemOrPair a)
   deriving (Show, Eq)

-- |Sequence of Elements and Pairs
-- Isomorphic to `[BinaryLeafTree a]` / `[ElemOrPair a]` / (`Cofree (Free Pair) a`?)
data USeq a = UNil | UCons (ElemOrPair a) (USeq a)

sizeEP :: ElemOrPair a -> Int
sizeEP (Elem _)     = 1
sizeEP (Pair xs ys) = sizeEP xs + sizeEP ys

sizeU :: USeq a -> Int
sizeU UNil         = 0
sizeU (UCons ep xs) = sizeEP ep + sizeU xs

---------------------------------------------------------------------------------------------------
-- Uniform vs Non-Uniform

{-

Uniform and Non-Uniform are NOT isomorphic, but can be converted between eachother.

# Pros of Non-Uniform
-------------------
* More concise
   * Less constructor noise in patterns and construction
* May require inlining for efficient representation
* The type in the non-uniform definition ensures that the outermost cons cell contains a single element,
  the second a pair of elements, the third a pair of pairs of elements, and so on.
   * The type in the uniform definition ensures neither that pairs are balanced nor that the nesting depth
   of pairs increases by one per level.
   * Example
      * Non-Uniform always   = E : (E, E) : ((E,E), (E,E)) : (((E,E), (E,E)), ((E,E), (E,E))) : ...
      * Non-Uniform can      = E : (E, E) : ((E,E), (E,E)) : (((E,E), (E,E)), ((E,E), (E,E))) : ...
      * Non-Uniform can also = ((E,E), (E,E)) : E : E : (E, (E, E)) : ...
-}
