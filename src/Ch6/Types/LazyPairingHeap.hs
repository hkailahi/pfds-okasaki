{-# LANGUAGE StrictData #-}

module Ch6.Types.LazyPairingHeap where

import BasicPrelude

import Ch3.Classes.Heap
  (Heap (deleteMin, empty, findMin, insert, isEmpty, merge), HeapEmpty (HeapEmpty))

data PairingHeap a =
    Ee
  | Tt a [PairingHeap a] -- ^ Well formed invariant: [PairingHeap a] cannot hold empty (`Ee`)
  deriving (Show, Eq, Functor, Foldable)

-- |Used on merge for non-lazy pairing heaps
-- Deleting the minimum element throws away the root and then merges the children in pairs
-- AKA Deleting minimum element twice means `mergePairs` would be called twice
--
-- Pairing heaps get their name from the deleteMin operation. deleteMin discards the root
-- and then merges the children in two passes. The first pass merges children in pairs from
-- left to right (i.e., the first child with the second, the third with the fourth, and so on).
-- The second pass merges the resulting trees from right to left.
mergePairs :: (Ord a) => [PairingHeap a] -> PairingHeap a
mergePairs []         = Ee
mergePairs [h]        = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

instance Heap PairingHeap where
  empty = Ee

  isEmpty Ee = True
  isEmpty _ = False

  insert x h = merge (Tt x []) h

  merge h Ee = h
  merge Ee h = h
  merge h1@(Tt x hs1) h2@(Tt y hs2) =
     if x < y
       then Tt x (h2 : hs1)
       else Tt y (h1 : hs2)

  findMin Ee       = Left $ HeapEmpty
  findMin (Tt x _) = Right x

  deleteMin Ee        = Left $ HeapEmpty
  deleteMin (Tt _ hs) = Right $ mergePairs hs

-------------------------------------------------------------------------------------------------

-- |Extra Heap field in each node to hold any partnerless children.
-- If there are no partnerless children (i.e., if the number of children is even), then this
-- extra field is empty.
data LazyPairingHeap a =
    E
  | T a (LazyPairingHeap a) ~(LazyPairingHeap a) -- 1st is "odd field", 2nd is child heap
  deriving (Show, Eq, Functor, Foldable)

-- |Used on merge for lazy pairing heaps
-- Represent the children of a node as a Heap suspension
link :: (Ord a) => LazyPairingHeap a -> LazyPairingHeap a -> LazyPairingHeap a
link (T x E m) a = T x a m
link (T x b m) a = T x E $ merge (merge a b) m
link _         _ = error "Pairing heaps are well-formed, child heaps cannot be empty"

instance Heap LazyPairingHeap where
  -- | O(1)
  empty :: LazyPairingHeap a
  empty = E

  -- | O(1)
  isEmpty :: LazyPairingHeap a -> Bool
  isEmpty E = True
  isEmpty _ = False

  -- | O(1)
  insert :: (Ord a) => a -> LazyPairingHeap a -> LazyPairingHeap a
  insert x a = merge (T x E E) a

  -- | O(log n) amortized
  merge :: (Ord a) => LazyPairingHeap a -> LazyPairingHeap a -> LazyPairingHeap a
  merge a E                     = a
  merge E b                     = b
  merge a@(T x _ _) b@(T y _ _)
    | x < y     = link a b
    | otherwise = link b a

  -- | O(1)
  findMin :: LazyPairingHeap a -> Either HeapEmpty a
  findMin E         = Left HeapEmpty
  findMin (T x _ _) = Right x

  -- | O(log n) amortized
  deleteMin :: (Ord a) => LazyPairingHeap a -> Either HeapEmpty (LazyPairingHeap a)
  deleteMin E          = Left HeapEmpty
  deleteMin (T _ a !m) = Right $ merge a m
