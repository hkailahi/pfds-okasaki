{-# LANGUAGE AllowAmbiguousTypes #-}

module Ch3.Classes.Heap where

import BasicPrelude hiding (empty, insert)

-- |Exception value representing an empty heap.
data HeapEmpty = HeapEmpty
  deriving (Eq, Show)

class Heap (s :: * -> *) where
  empty   :: s a
  isEmpty :: s a -> Bool

  insert :: (Ord a) =>   a -> s a -> s a
  merge  :: (Ord a) => s a -> s a -> s a

  findMin   ::             s a -> Either HeapEmpty a
  deleteMin :: (Ord a) =>  s a -> Either HeapEmpty (s a)

buildHeap :: (Heap h, Ord a) => [a] -> h a
buildHeap = foldl' (flip insert) empty -- equiv to foldr insert E . reverse

---------------------------------------------------------------------------------------------------

data MinPolicy
data MaxPolicy

class MergeOrd p where
  comparePriority :: Ord a => a -> a -> Bool

instance MergeOrd MinPolicy where
  comparePriority :: (Ord a) => a -> a -> Bool
  comparePriority x y = x < y

instance MergeOrd MaxPolicy where
  comparePriority :: (Ord a) => a -> a -> Bool
  comparePriority x y = x >= y
