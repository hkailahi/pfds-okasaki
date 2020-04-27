module Ch2.Ex2 where

import BasicPrelude

import Ch2.Types.Tree (Tree (E, T))
import Ch2.Types.UnbalancedSet (UnbalancedSet (BST))

-- Exercise 2.2 (Andersson [And91D]) In the worst case, `member` performs approximately `2d` comparisons, where `d`
-- is the depth of the tree. Rewrite `member` to take no more than `d + 1` comparisons by keeping track of a
-- candidate element that might be equal to the query element (say, the last element for which < returned false or
-- < returned true) and checking for equality only when you hit the bottom of the tree.

-- |2d comparisons, where d is depth of tree
-- 2d = 1 for (x < y) for lT + 1 for (x > y) for rT = input always goes to right/greater than any current value in tree
badMember :: Ord e => e -> Tree e -> Bool
badMember _ E           = False
badMember x (T lT y rT) | x < y     = badMember x lT
                        | x > y     = badMember x rT
                        | otherwise = True

-- |d + 1 comparisons, where d is depth of tree
-- `prev` refers to *a* previous member on the current path, but not necessarily the immediately
-- previous one
goldMember :: forall e. Ord e => e -> Tree e -> Bool
goldMember x t =
  let
    eqX :: Maybe e -> Bool
    eqX = \case
      Nothing   -> False      -- No previous
      Just prev -> x == prev  -- A previous member, rather than *the* previous member
    go :: Tree e -> Maybe e -> Bool
    go E           prev = eqX prev
    go (T lT y rT) prev
      | x < y     = go lT prev
      | otherwise = go rT $ Just y
  in go t Nothing

---------------------------------------------------------------------------------------------------

-- |https://github.com/stappit/okasaki-pfds/blob/master/src/Chap02/Exercise02.hs
goldMember' :: Ord a => a -> UnbalancedSet a -> Bool
goldMember' x t = go x t []
  where
    go :: Ord a => a -> UnbalancedSet a -> [a] -> Bool
    go a (BST E) bs       = a `elem` bs -- FIXME Clearly doing more comparisons
    go a (BST (T l b r)) bs
        | a < b     = go a (BST l) bs
        | otherwise = go a (BST r) (b:bs)
