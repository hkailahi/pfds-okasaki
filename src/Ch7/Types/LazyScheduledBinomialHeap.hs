{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Ch7.Types.LazyScheduledBinomialHeap where

import BasicPrelude

import Ch3.Classes.Heap
  ( Heap (empty, deleteMin, isEmpty, findMin, merge, insert)
  , HeapEmpty (HeapEmpty)
  )

-- FIXME Ignore all comments, they are leftover from regular BinomialHeap

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
  Node a [Tree a]
  deriving (Eq, Show, Functor, Foldable)

data Digit a =
    Zero
  | One (Tree a)
  deriving (Eq, Show, Functor, Foldable)

newtype Schedule a = Schedule
  { unSchedule :: [[Digit a]] }
  deriving (Eq, Show, Functor, Foldable)

exec :: Schedule a -> Schedule a
exec (Schedule [])                   = Schedule []
exec (Schedule ((One _ : _) : sched))  = Schedule sched
exec (Schedule ((Zero : job) : sched)) = Schedule $ job : sched
exec _ = error "blabadkamsdasdk"

-- |Links two binomial trees of rank r to get a binomial tree of rank r + 1.
-- This is done by making one tree the leftmost child of the other.
link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2  = Node x1 (t2:c1)
  | otherwise = Node x2 (t1:c2)

insTree :: (Ord a) => Tree a -> [Digit a] -> [Digit a]
insTree t []          = One t : []
insTree t (Zero:ds)   = One t : ds
insTree t (One t':ds) = Zero  : insTree (link t t') ds

mrgTrees :: (Ord a) => [Digit a] -> [Digit a] -> [Digit a]
mrgTrees h1         []         = h1
mrgTrees []         h2         = h2
mrgTrees (Zero:xs)  (y:ys)     = y : mrgTrees xs ys
mrgTrees (x:xs)     (Zero:ys)  = x : mrgTrees xs ys
mrgTrees (One x:xs) (One y:ys) = Zero : insTree (link x y) (mrgTrees xs ys)

normalize :: [a] -> [a]
normalize []     = []
normalize (_:xs) = normalize xs

popMinTree :: (Ord a) => [Digit a] -> (Tree a, [Digit a])
popMinTree (One t : [])   = (t, [])
popMinTree (Zero  : ts)   =
  let (t', ds') = popMinTree ts
  in  (t', Zero : ds')
popMinTree (One t@(Node x _) : ts)   =
  case popMinTree ts of
    (t'@(Node x' _), ts') ->
      if x <= x'
        then (t, Zero:ts)
        else (t', One t:ts')
popMinTree _ = error "lalalalala"

-- ---------------------------------------------------------------------------------------------------

-- |A binomial heap is a collection of heap-ordered binomial trees in which no two trees have the
-- same rank. This collection is represented as a list of trees in increasing order of rank.
-- This is a lazy scheduled Binomial Heap.
data LSBH a = LSBH
  { heap :: [Digit a]
  , jobs :: Schedule a
  }
  deriving (Eq, Show, Functor, Foldable)

instance (forall a. Ord a) => Heap LSBH where
  -- |O(1) worst case
  empty :: LSBH a
  empty = LSBH [] (Schedule [])

  -- |O(1) worst case
  isEmpty :: LSBH a -> Bool
  isEmpty (LSBH [] (Schedule [])) = True
  isEmpty _                       = False

  -- |Amortized time O(1), worst case O(log n)
  -- The worst case is insertion into a heap of size n = 2^k — 1, requiring a total of k
  -- links and O(k) = O(log n) time.
  insert :: (Ord a) => a -> LSBH a -> LSBH a
  insert x (LSBH ds (Schedule sched)) =
    let ds' = insTree (Node x []) ds
    in LSBH ds' . exec . exec . Schedule $ ds' : sched

  -- |Amortized time O(log n), worst case O(log n)
  -- This data structure does not make this operation fast
  -- To merge two heaps, we step through both lists of trees in increasing order of rank, linking
  -- trees of equal rank as we go. Again, each link corresponds to a carry in binary arithmetic.
  merge :: (Ord a) => LSBH a -> LSBH a -> LSBH a
  merge (LSBH h1 _) (LSBH h2 _) =
    let ds = normalize $ mrgTrees h1 h2
    in LSBH ds (Schedule [])

  -- |Takes the same amount of time as deleteMin, since it walks left branches.
  -- Calls auxiliary `popMinTree` that finds the tree with the minimum root and removes it from
  -- the list, returning both the tree and the remaining list.
  findMin :: (Ord a) => LSBH a -> Either HeapEmpty a
  findMin (LSBH [] _) = Left HeapEmpty
  findMin (LSBH xs _) =
    let (Node x _, _) = popMinTree xs
    in Right x

  -- |Amortized time O(log n), worst case O(log n)
  -- Calls auxiliary `popMinTree` that finds the tree with the minimum root and removes it from
  -- the list, returning both the tree and the remaining list.
  deleteMin :: (Ord a) => LSBH a -> Either HeapEmpty (LSBH a)
  deleteMin (LSBH [] _)     = Left HeapEmpty
  deleteMin (LSBH xs _) =
    let (Node _ ts1, ts2) = popMinTree xs
        ds'' = mrgTrees (map One (reverse ts1)) ts2
    in Right $ LSBH (normalize ds'') (Schedule [])
