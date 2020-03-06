{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Ch6.Types.LazyBinomialHeap where

import BasicPrelude
import Data.List.NonEmpty (NonEmpty ((:|)))

import Ch3.Classes.Heap
  ( Heap (empty, deleteMin, isEmpty, findMin, merge, insert)
  , HeapEmpty (HeapEmpty)
  )

---------------------------------------------------------------------------------------------------

data Tree a =
  Node Int a [Tree a]
  deriving (Eq, Show, Functor, Foldable)

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2:c1)
  | otherwise = Node (r + 1) x2 (t1:c2)

insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insTree x []       = [x]
insTree x t@(y:ys)
  | rank x < rank y = x:t
  | otherwise       = insTree (link x y) ys

mrgTrees :: (Ord a) => [Tree a] -> [Tree a] -> [Tree a]
mrgTrees h1        []        = h1
mrgTrees []        h2        = h2
mrgTrees h1@(x:xs) h2@(y:ys)
  | rank x < rank y = x : mrgTrees xs h2
  | rank y < rank x = y : mrgTrees h1 ys
  | otherwise       = insTree (link x y) $ mrgTrees xs ys

popMinTree :: (Ord a) => NonEmpty (Tree a) -> (Tree a, [Tree a])
popMinTree (t :| [])   = (t, [])
popMinTree (t :| [x])  = if root t <= root x then (t, [x]) else (x, [t])
popMinTree (t :| x:xs) =
  let (t', ts') = popMinTree $ x :| xs
  in
    if root t <= root t'
      then (t, xs)
      else (t', x : ts')

---------------------------------------------------------------------------------------------------

newtype LazyBinomialHeap a = LazyBinomialHeap
  { unLazyBinomialHeap :: [Tree a] }
  deriving (Eq, Show, Functor, Foldable)

instance (forall a. Ord a) => Heap LazyBinomialHeap where
  -- O(1) worst case
  empty :: LazyBinomialHeap a
  empty = LazyBinomialHeap []

  -- O(1) worst case
  isEmpty :: LazyBinomialHeap a -> Bool
  isEmpty (LazyBinomialHeap []) = True
  isEmpty _                 = False

  -- Amortized time O(log n), proof below
  insert :: (Ord a) => a -> LazyBinomialHeap a -> LazyBinomialHeap a
  insert x = LazyBinomialHeap . insTree (Node 0 x []) . unLazyBinomialHeap

  -- This is up to O(n), this data structure does not make this operation fast
  merge :: (Ord a) => LazyBinomialHeap a -> LazyBinomialHeap a -> LazyBinomialHeap a
  merge (LazyBinomialHeap h1) (LazyBinomialHeap h2) = LazyBinomialHeap $ mrgTrees h1 h2

  -- Takes the same amount of time as deleteMin, since it walks left branches.
  findMin :: (Ord a) => LazyBinomialHeap a -> Either HeapEmpty a
  findMin (LazyBinomialHeap [])     = Left HeapEmpty
  findMin (LazyBinomialHeap (x:xs)) = Right . root . fst . popMinTree $ x :| xs

  -- Amortized time O(log n), proof below
  deleteMin :: (Ord a) => LazyBinomialHeap a -> Either HeapEmpty (LazyBinomialHeap a)
  deleteMin (LazyBinomialHeap [])     = Left HeapEmpty
  deleteMin (LazyBinomialHeap (x:xs)) =
    let ((Node _ _ ts1), ts2) = popMinTree $ x :| xs
    in Right . LazyBinomialHeap $ mrgTrees (reverse ts1) ts2
