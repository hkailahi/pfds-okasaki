module Ch9.Types.SparseRandomAccessList where

import BasicPrelude hiding (empty)

import Ch9.Classes.RandomAccessList
import Ch9.Types.DenseRandomAccessList
  (CBLeafTree (CBLeaf, CBNode), link, lookupTree, size, updateTree)

-- |EXERCISE 9.3

-- |Implementation of random access lists using sparse by weight binary numbers of complete
-- binary leaf trees.

newtype SparseRAList a = SparseRAList [CBLeafTree a]
  deriving (Eq, Show)

-- As with sparse lists, unconsTree ultimately returns a (head, tail) pair.
unconsTree :: [CBLeafTree a] -> Either Empty (a, [CBLeafTree a])
unconsTree = \ case
  []   -> Left Empty
  t:ts -> let (x, ts') = unconsSingle t in Right (x, reverse ts' ++ ts)
  where
    unconsSingle :: CBLeafTree a -> (a, [CBLeafTree a])
    unconsSingle = \ case
      CBLeaf x      -> (x, [])
      CBNode _ u u' -> let (x, us') = unconsSingle u in (x, u':us')

instance RandomAccessList SparseRAList where

  empty = SparseRAList []

  isEmpty (SparseRAList []) = True
  isEmpty _                 = False

  cons x (SparseRAList tts) = SparseRAList $ consTree (CBLeaf x) tts
    where
      -- consTree is called starting from size 1, so the size t > size t' case is excluded
      consTree :: CBLeafTree a -> [CBLeafTree a] -> [CBLeafTree a]
      consTree t []          = [t]
      consTree t ts@(t':ts')
        | size t < size t'   = t:ts
        | size t == size t'  = consTree (link t t') ts'
        | otherwise          = error "forbidden by invariant of consTree"

  head (SparseRAList tts) = fst <$> unconsTree tts

  tail (SparseRAList tts) = SparseRAList . snd <$> unconsTree tts

  lookup i (SparseRAList tts) = lookup' i tts
    where
      lookup' :: Integer -> [CBLeafTree a] -> Either IndexError a
      lookup' j = if j < 0 then \_ -> Left IndexError else \ case
        []   -> Left IndexError
        t:ts
          | j < size t -> lookupTree j t
          | otherwise  -> lookup' (j - size t) ts

  update i x (SparseRAList tts) = SparseRAList <$> update' i x tts
    where
      update' :: Integer -> a -> [CBLeafTree a] -> Either IndexError [CBLeafTree a]
      update' j y = if j < 0 then \_ -> Left IndexError else \ case
        []             -> Left IndexError
        t:ts
          | j < size t -> do
              updatedFirst <- updateTree j y t
              pure $ updatedFirst : ts
          | otherwise  -> (t :) <$> update' (j - size t) y ts
