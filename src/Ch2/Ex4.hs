module Ch2.Ex4 where

import BasicPrelude hiding (insert)

import Ch2.Ex2 (goldMember)
import Ch2.Ex3 (insertNoCopyOnExisting)
import Ch2.Types.Tree (Tree (E, T))
import Ch2.Types.UnbalancedSet (UnbalancedSet (BST, unBST))

-- Exercise 2.4 Combine the ideas of the previous two exercises to obtain a version of insert that
-- performs no unnecessary copying and uses no more than d + 1 comparisons.

goldInsert :: forall a. (Ord a) => a -> Tree a -> Tree a
goldInsert x t = case goldMember x t of
  True  -> t
  False -> insertNoCopyOnExisting x t

---------------------------------------------------------------------------------------------------

-- |https://github.com/stappit/okasaki-pfds/blob/master/src/Chap02/Exercise04.hs
insert' :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert' x t = maybe t BST $ go x (unBST t) []
  where
    go :: Ord a => a -> Tree a -> [(Tree a, a)] -> Maybe (Tree a)
    go x' E ls     = mk x' (Just $ T E x' E) ls
    go x' (T l y r) ls
      | x' < y    = mk x' (T <$> go x' l [] <*> pure y <*> pure r) ls -- 2
      | otherwise = go x' r $ (l, y):ls                               -- 3

    -- FIXME Again, clearly too many comparisons
    mk :: Eq a => a -> Maybe (Tree a) -> [(Tree a, a)] -> Maybe (Tree a)
    mk x' = foldl $ \mr (l, y) ->
      if x' == y
        then Nothing
        else T l y <$> mr