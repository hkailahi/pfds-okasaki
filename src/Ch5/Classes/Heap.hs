module Ch5.Classes.Heap where

import BasicPrelude

-- |Exception value representing an empty heap.
data HeapEmpty = HeapEmpty
  deriving (Eq, Show)

-- |Heap signature from Figure 3.1
class (Ord a) => Heap f a where

  empty :: f a
  -- ^ An empty heap.

  isEmpty :: f a -> Bool
  -- ^ True if the heap is empty, False otherwise.

  insert :: a -> f a -> f a
  -- ^ Add an element to a heap.

  merge :: f a -> f a -> f a
  -- ^ Combine two heaps into one.

  findMin :: f a -> Either HeapEmpty a
  -- ^ Yield the minimal element of the heap, or HeapEmpty if heap is empty.

  deleteMin :: f a -> Either HeapEmpty (f a)
  -- ^ Yield the heap with minimal element removed, or Heap Empty if heap is empty.
