module Ch5.SplayHeap where

import BasicPrelude hiding (partition)

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

-- |Simple binary tree with labels on branch nodes.
--
-- The tree is enforced to be a valid binary search tree in the sense that for a tree
-- Node l x r, every node label in l is <= x and every node label in r is >= x.
-- I don't have enough type-fu to make the type enforce this.
data Tree a =
    Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

-- |Exercise 5.4. Helper functions for splay tree partitioning.

-- |Given a tree t and an element (called pivot by analogy with quicksort), return a tree
-- containing those elements of t which are > pivot.
--
-- Rebalance in the process to reduce tree depth: if when looking for the balance point we
-- follow a left branch twice in a row, rearrange the tree to hang the lower root from the
-- point where the upper root was: if we are looking for 25,
--
--     50    becomes  30
--    /  \           /  \
--   30  70         10  50
--  /  \               /  \
-- 10  40             40  70
bigger :: (Ord a) => a -> Tree a -> Tree a
bigger _     Leaf           = Leaf
bigger pivot t@(Node l x r)
  | x <= pivot = bigger pivot r
  | x > pivot  = case l of
      Leaf          -> t
      Node l' x' r'
        | x' <= pivot -> Node (bigger pivot r') x r  -- filter-the-right case
        | x' > pivot  -> Node (bigger pivot l') x' (Node r' x r)  -- rebalance case
      _             -> error "Partial - Non-exhaustive pattern match"
bigger _ _ = error "Partial - Non-exhaustive pattern match"

-- |Same idea, opposite comparison. Equal signs move around because `smaller` should
-- extract the subtree of elements <= pivot, not < pivot.
smaller :: (Ord a) => a -> Tree a -> Tree a
smaller _     Leaf           = Leaf
smaller pivot t@(Node l x r)
  | x > pivot  = smaller pivot l
  | x <= pivot = case r of
      Leaf          -> t
      Node l' x' r'
        | x' > pivot  -> Node l x (smaller pivot l')  -- filter-the-left case
        | x' <= pivot -> Node (Node l x l') x' (smaller pivot r')  -- rebalance case
      _             -> error "Partial - Non-exhaustive pattern match"
smaller _ _ = error "Partial - Non-exhaustive pattern match"

-- |Combine the effect of smaller and bigger into a single pass function.
-- partition pivot t = (small, big) where small contains all the elements of t that are
-- <= pivot and big contains all the elements of t that are > pivot.
partition :: (Ord a) => a -> Tree a -> (Tree a, Tree a)
partition _     Leaf           = (Leaf, Leaf)
partition pivot t@(Node l x r)
  | x <= pivot = case r of
      Leaf          -> (t, Leaf)
      Node l' x' r'
        | x' > pivot  -> let (small, big) = partition pivot l'
                         in (Node l x small, Node big x' r')
        | x' <= pivot -> let (small, big) = partition pivot r'  -- rebalance case
                         in (Node (Node l x l') x' small, big)
      _             -> error "Partial - Non-exhaustive pattern match"
  | x > pivot  = case l of
      Leaf          -> (Leaf, t)
      Node l' x' r'
        | x' <= pivot -> let (small, big) = partition pivot r'
                         in (Node l' x' small, Node big x r)
        | x' > pivot  -> let (small, big) = partition pivot l'  -- rebalance case
                         in (small, Node big x' (Node r' x r))
      _             -> error "Partial - Non-exhaustive pattern match"
partition _     _              = error "Partial - Non-exhaustive pattern match"


instance (Ord a) => Heap Tree a where
  -- O(1) worst case
  empty = Leaf

  -- O(1) worst case
  isEmpty Leaf    = True
  isEmpty Node {} = False

  -- Amortized time O(log n), proof below
  insert x t = let (small, big) = partition x t in Node small x big

  -- This is up to O(n), this data structure does not make this operation fast
  merge t Leaf            = t
  merge t (Node l' x' r') =
    let (l, r) = partition x' t in Node (merge l l') x' (merge r r')

  -- Takes the same amount of time as deleteMin, since it walks left branches.
  -- Therefore if we wrap this in a memoizer (not done here) which is cleared by deleteMin, we
  -- can assume it gets called at most once between calls to deleteMin, and therefore it obeys
  -- the same amortized time estimate as max(insert, deleteMin).
  findMin Leaf            = Left HeapEmpty
  findMin (Node Leaf x _) = Right x
  findMin (Node l _ _)    = findMin l

  -- Amortized time O(log n), proof below
  deleteMin Leaf                       = Left HeapEmpty
  deleteMin (Node Leaf            _ r) = Right r
  deleteMin (Node (Node l' x' r') x r) = Right $ case deleteMin l' of
    Left _    -> Node r' x r  -- x' is the minimum element
    Right l'' -> Node l'' x' (Node r' x r)  -- rebalance case

-- |ANALYSIS OF SplayHeap
--
-- Define the potential V as follows:
--
-- 1. For a node (subtree) t, define #t = 1 + size (node count) of t.
-- So #Leaf = 2, and #(Node l x r) = #l + #r (because the overcounts of root node for l and r
-- become a count and overcount of the root node of the combination). This additive property
-- makes computation much easier.
--
-- 2. For a node (subtree) t, define u(t) = log(#t) (binary logarithm, but up to constant
-- factors it doesn't matter).
--
-- 3. For a tree t, define V(t) = sum(u(n): n is a node of t).
--
-- To get a feel for how this behaves, consider two trees with m = 2^k - 1 nodes.
--
-- * The tree t with one linear branch has #n = 2 at the end, #n = 3 for its parent, and so
-- on up to #n = 2^k at the root. So
--
-- V(t) = sum(log(j) : 1 < j <= 2^k) = log((2^k)!)
--
-- Stirling's approximation says log(m!) ~ m log m - m + O(m). So this gives us
--
-- V(t) = Theta(k 2^k) = Theta(m log m)
--
-- * The complete binary tree with 2^k - 1 nodes (k levels) has 2^{k-1} nodes with #n = 2
-- and thus u(n) = 1; 2^{k-2} nodes with #n = 4 and so u(n) = 2; 2^{k-3} nodes with #n = 8
-- and so u(n) = 3; and so on up to 1 node with #n = 2^k, u(n) = k. So
--
-- V(t) = sum(j 2^{k-j} : 0 < j <= k)
--
-- We can evaluate this sum by unstacking the triangle:
--
-- V(t) = sum(sum(2^{k-j} : s < j <= k) : 0 <= s < k)
--      = sum(2^{k-s} : 0 <= s < k)
--      = 2^{k+1} - 2.
--
-- So instead of Theta(m log m), we get V(t) = Theta(m) (in fact exactly 2m).
--
-- Now, to prove our estimates, we need a simple lemma:
--
-- [Lemma 5.1] If x, y, z > 0 and y + z <= x, then 1 + log y + log z < 2 log x.
--             (This is specific to binary logarithms.)
--
-- Proof. Assume without loss of generality that y <= z. Then y <= x/2, so
-- 1 + log y <= log x; and z < x, so log z < log x. Add these two inequalities.
--
-- To get an amortized estimate for `insert`, we need to estimate the cost of `partition`.
-- Because `partition` takes constant time plus recursive calls, it is enough to define the
-- actual running time T(t) of `partition` to be the number of calls to `partition` in the
-- execution trace of `partition t`.
--
-- Then if partition t = (a, b), the amortized cost A(t) of calling `partition t` becomes
-- A(t) = T(t) + V(a) + V(b) - V(t) (the true cost plus the amount of potential we charged).
--
-- [Theorem 5.2] A(t) <= 1 + 2 u(t) = 1 + 2 log(#t).
--
-- Proof is by structural induction on t and splits into two analyses depending on whether we
-- are in one of the two rebalancing branches of partition or the non-rebalancing branches.
--
-- For the rebalancing case, we are transforming as follows: (or its mirror image)
--
--   s -> x                  y <- s'
--       / \                / \
-- t -> y   d  ==>  a  ||  b   x <- t'    where partition r = (a, b)
--     / \                    / \
--    r   c                  c   d
--
-- Here we compute:
--
-- A(s) = T(s)     + V(a) + V(s') - V(s)
--      = 1 + T(r) + V(a) + V(s') - V(s)
--      = 1 + A(r) - V(a) - V(b) + V(r) + V(a) + V(s') - V(s)
--      = 1 + A(r) + V(r) - V(b)
--        + (u(s') + V(b) + u(t') + V(c) + V(d))  # V(s')
--        - (u(s) + u(t) + V(r) + V(c) + V(d))    # V(s)
--      = 1 + A(r) + u(s') + u(t') - u(s) - u(t)
--     <= 2 + 2 u(r) + u(s') + u(t') - u(s) - u(t)  # inductive hypothesis on A(r)
-- Now u(s') <= u(s) because b has fewer nodes than r, and u(r) < u(t) for the same reason, so
--      < 2 + u(r) + u(t')
--      < 1 + 2 u(s)  # since #r + #t' = #r + #c + #d = #s, apply the lemma
-- as required.
--
-- [Exercise 5.5] In the non-rebalancing case, we instead have: (or its mirror image)
--
--   s -> x     s' -> y          x <- t'
--       / \         / \        / \
-- t -> y   d  ==>  c   a  ||  b   d    where partition r = (a, b)
--     / \
--    c   r
--
-- Now compute:
--
-- A(s) = T(s)     + V(s') + V(t') - V(s)
--      = 1 + T(r) + V(s') + V(t') - V(s)
--      = 1 + A(r) - V(a) - V(b) + V(r) + V(s') + V(t') - V(s)
--      = 1 + A(r) - V(a) - V(b) + V(r)
--        + (u(s') + V(c) + V(a))
--        + (u(t') + V(b) + V(d))
--        - (u(s) + u(t) + V(c) + V(d) + V(r))
--      = 1 + A(r) + u(s') + u(t') - u(s) - u(t)
--     <= 2 + 2 u(r) + u(s') + u(t') - u(s) - u(t)  # inductive hypothesis on A(r)
-- Now u(r) < u(t) because #r <= #t - 1, so
--      < 2 + u(r) + u(s') + u(t') - u(s).
-- And #s = #r + #c + #d = (#a + #b - 1) + #c + #d = #(s') + #(t') - 1, that is
-- #(s') + #(t') = #s + 1. The lemma then gives 1 + u(s') + u(t') < 2 log(#s + 1), so
--      < 1 + (u(r) - u(s)) + 2 log(#s + 1).
-- We want instead
--      < 1 + 2 u(s),
-- which will follow if we can show that
--   2(log(#s + 1) - log(#s)) < log(#s) - log(#r).
-- Because #r <= #s - 2, this is implied by
--   2(log(#s + 1) - log(#s)) < log(#s) - log(#s - 2)
-- and this is true since the derivative of log is decreasing.
--
-- [Exercise 5.6] deleteMin also has amortized running time O(log n).
--
-- Proof. As before, let T(s) be the actual and A(s) the amortized running time of deleteMin,
-- counted as number of recursive calls since the cost above recursive calls is constant.
-- Specifically, claim that A(s) < 1 + 2 u(s).
--
-- The non-recursive cases and the non-rebalance recursive cases all take constant time (the
-- latter because `deleteMin Leaf` takes constant time).
--
-- In the rebalancing recursive case:
--
--   s -> x       s' -> x'
--       / \           / \
-- t -> x'  r  ==>   l''  x <- t'   where deleteMin l' = l''
 --    / \               / \
--    l' r'             r'  r
--
-- So A(s) = T(s)      + V(s') - V(s)
--         = 1 + T(l') + V(s') - V(s)
--         = 1 + A(l') - V(l'') + V(l') + V(s') - V(s)
--         = 1 + A(l') - V(l'') + V(l')
--           + (u(s') + u(t') + V(r) + V(r') + V(l''))
--           - (u(s)  + u(t)  + V(r) + V(r') + V(l'))
--         = 1 + A(l') + u(s') + u(t') - u(s) - u(t)
--         < 2 + 2 u(l') + u(s) + u(t') - u(s) - u(t)  # inductive hypothesis
-- As before, estimate u(l') < u(t) and u(s') < u(s) to get
--         < 2 + u(l') + u(t')
-- and #(l'') + #(t') < #(s) so the lemma applies to yield
--         < 1 + 2 u(s)
-- as required.
