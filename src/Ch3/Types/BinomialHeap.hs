{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Ch3.Types.BinomialHeap where

import BasicPrelude
import Data.List.NonEmpty (NonEmpty ((:|)))

import Ch3.Classes.Heap
  ( Heap (empty, deleteMin, isEmpty, findMin, merge, insert)
  , HeapEmpty (HeapEmpty)
  )

---------------------------------------------------------------------------------------------------

-- |Binomial Trees represented as an int-ranked n-ary tree
--
-- Binomial Trees are inductively defined as follows:
-- • A binomial tree of rank 0 is a singleton node.
-- • A binomial tree of rank r + 1 is formed by linking two binomial trees of rank r,
--   making one tree the leftmost child of the other.
--
-- A binomial tree of rank r contains exactly 2^r nodes. In other words, a binomial tree of rank r
-- is a node with r children t_1...t_r , where each t_i is a binomial tree of rank r — i.
data Tree a =
  Node !Int !a ![Tree a]
  deriving (Eq, Show, Functor, Foldable)

-- |O(1) Finds the rank of an n-ary tree
-- The rank of a node is defined to be the length of its right spine (i.e., the rightmost path from
-- the node in question to an empty node).
rank :: Tree a -> Int
rank (Node r _ _) = r

-- |O(1) Finds the root element of an n-ary tree
root :: Tree a -> a
root (Node _ x _) = x

-- |Links two binomial trees of rank r to get a binomial tree of rank r + 1.
-- This is done by making one tree the leftmost child of the other.
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

-- |A binomial heap is a collection of heap-ordered binomial trees in which no two trees have the
-- same rank. This collection is represented as a list of trees in increasing order of rank.
newtype BinomialHeap a = BinomialHeap
  { unBinomialHeap :: [Tree a] }
  deriving (Eq, Show, Functor, Foldable)

instance (forall a. Ord a) => Heap BinomialHeap where
  -- |O(1) worst case
  empty :: BinomialHeap a
  empty = BinomialHeap []

  -- |O(1) worst case
  isEmpty :: BinomialHeap a -> Bool
  isEmpty (BinomialHeap []) = True
  isEmpty _                 = False

  -- |Amortized time O(1), worst case O(log n)
  -- The worst case is insertion into a heap of size n = 2^k — 1, requiring a total of k
  -- links and O(k) = O(log n) time.
  insert :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
  insert x = BinomialHeap . insTree (Node 0 x []) . unBinomialHeap

  -- |Amortized time O(log n), worst case O(log n)
  -- This data structure does not make this operation fast
  -- To merge two heaps, we step through both lists of trees in increasing order of rank, linking
  -- trees of equal rank as we go. Again, each link corresponds to a carry in binary arithmetic.
  merge :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
  merge (BinomialHeap h1) (BinomialHeap h2) = BinomialHeap $ mrgTrees h1 h2

  -- |Takes the same amount of time as deleteMin, since it walks left branches.
  -- Calls auxiliary `popMinTree` that finds the tree with the minimum root and removes it from
  -- the list, returning both the tree and the remaining list.
  findMin :: (Ord a) => BinomialHeap a -> Either HeapEmpty a
  findMin (BinomialHeap [])     = Left HeapEmpty
  findMin (BinomialHeap (x:xs)) = Right . root . fst . popMinTree $ x :| xs

  -- |Amortized time O(log n), worst case O(log n)
  -- Calls auxiliary `popMinTree` that finds the tree with the minimum root and removes it from
  -- the list, returning both the tree and the remaining list.
  deleteMin :: (Ord a) => BinomialHeap a -> Either HeapEmpty (BinomialHeap a)
  deleteMin (BinomialHeap [])     = Left HeapEmpty
  deleteMin (BinomialHeap (x:xs)) =
    let (Node _ _ ts1, ts2) = popMinTree $ x :| xs
    in  Right . BinomialHeap $ mrgTrees (reverse ts1) ts2
