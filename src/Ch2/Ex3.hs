module Ch2.Ex3 where

import BasicPrelude hiding (insert)

import Ch2.Classes.Set (Set (insert, member))
import Ch2.Types.Tree (Tree (E, T))
import Ch2.Types.UnbalancedSet (UnbalancedSet (BST, unBST))

-- Exercise 2.3 Inserting an existing element into a binary search tree copies the entire search
-- path even though the copied nodes are indistinguishable from the originals. Rewrite insert
-- using exceptions to avoid this copying. Establish only one handler per insertion rather than one
-- handler per iteration.

insertNoCopyOnExisting :: (Ord a) => a -> Tree a -> Tree a
insertNoCopyOnExisting x t = case member x t of
  True  -> t
  False -> insert x t

---------------------------------------------------------------------------------------------------

-- |https://github.com/stappit/okasaki-pfds/blob/master/src/Chap02/Exercise03.hs
insert' :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert' x t = maybe t BST . go x $ unBST t
  where
    go :: Ord a => a -> Tree a -> Maybe (Tree a)
    go y E = Just $ T E y E
    go y (T l z r)
      | y < z     = T <$> go y l <*> pure z <*> pure r
      | y > z     = T l z <$> go y r
      | otherwise = Nothing
