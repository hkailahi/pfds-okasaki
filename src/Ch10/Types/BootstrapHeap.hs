{-# LANGUAGE UndecidableInstances #-}

module Ch10.Types.BootstrapHeap where

import BasicPrelude hiding (empty, insert)

import Ch5.Classes.Heap

-- `CoFree :+: ()`
data BootstrapHeap h a =
    E
  | H a (h (BootstrapHeap h a))
deriving instance (Show a, Show (h (BootstrapHeap h a))) => Show (BootstrapHeap h a)
deriving instance (Eq a, Eq (h (BootstrapHeap h a))) => Eq (BootstrapHeap h a)

-- instance Eq a => Eq (BootstrapHeap h a) where
--   (H x _) == (H y _) = (x == y)
--   E == E             = True
--   _ == _             = False

-- instance Ord a => Ord (BootstrapHeap h a) where
--   (H x _) < (H y _) = (x < y)

unit :: (Heap h (BootstrapHeap h a)) => a -> BootstrapHeap h a
unit x = H x empty

instance (Heap h (BootstrapHeap h a), Ord a)
  => Heap (BootstrapHeap h) a where
  empty :: BootstrapHeap h a
  empty = E

  -- |O(1) worst case
  isEmpty :: BootstrapHeap h a -> Bool
  isEmpty E = True
  isEmpty _ = False

  -- |O(1) worst case
  insert :: a -> BootstrapHeap h a -> BootstrapHeap h a
  insert x h = merge (unit x) h

  -- |O(1) worst case (down from O(log n) worst case)
  merge :: BootstrapHeap h a -> BootstrapHeap h a -> BootstrapHeap h a
  merge E h = h
  merge h E = h
  merge h1@(H x p1) h2@(H y p2) =
    if x < y
      then H x (insert h2 p1)
      else H y (insert h1 p2)

  -- |O(1) worst case (down from O(log n) worst case)
  findMin :: BootstrapHeap h a -> Either HeapEmpty a
  findMin E       = Left HeapEmpty
  findMin (H x _) = Right x

  -- |O(log n) worst case
  deleteMin :: BootstrapHeap h a -> Either HeapEmpty (BootstrapHeap h a)
  deleteMin E = Left HeapEmpty
  deleteMin (H _ p) =
    if isEmpty p
      then Right E
      else let Right (H y p1) = findMin p
               Right p2       = deleteMin p
           in  Right $ H y (merge p1 p2)