module Ch6.Types.LazyPairingHeap where

import BasicPrelude

import Ch3.Classes.Heap
  (Heap (deleteMin, empty, findMin, insert, isEmpty, merge), HeapEmpty (HeapEmpty))

data PairingHeap a =
    E
  | T a (PairingHeap a) (PairingHeap a)
  deriving (Show, Eq, Functor, Foldable)

link :: (Ord a) => PairingHeap a -> PairingHeap a -> PairingHeap a
link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)
link _         _ = error "TODO"

instance Heap PairingHeap where
  empty :: PairingHeap a
  empty = E

  isEmpty :: PairingHeap a -> Bool
  isEmpty E = True
  isEmpty _ = False

  insert :: (Ord a) => a -> PairingHeap a -> PairingHeap a
  insert x a = merge (T x E E) a

  merge :: (Ord a) => PairingHeap a -> PairingHeap a -> PairingHeap a
  merge a E                     = a
  merge E b                     = b
  merge a@(T x _ _) b@(T y _ _)
    | x < y     = link a b
    | otherwise = link b a

  findMin :: PairingHeap a -> Either HeapEmpty a
  findMin E         = Left HeapEmpty
  findMin (T x _ _) = Right x

  deleteMin :: (Ord a) => PairingHeap a -> Either HeapEmpty (PairingHeap a)
  deleteMin E         = Left HeapEmpty
  deleteMin (T _ a m) = Right $ merge a m
