module Ch5.Queue where

import BasicPrelude

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- |AMORTIZATION
--
-- The basic inequality for amortized cost estimates to be valid is to show that, if
-- an operation consists of a sequence [o_1, ..., o_n] of primitive operations, and we
-- have an estimated cost a_j for operation o_j, and t_j represents the actual cost of
-- operation o_j, then
--
-- sum(a_j : 1 <= j <= n) >= sum(t_j : 1 <= j <= n)
--
-- Usually inductive proofs are easier if we strengthen the result so that we are never
-- in deficit at any point:
--
-- sum(a_j : 1 <= j <= k) >= sum(t_j : 1 <= j <= k) for every 1 <= k <= n
--
-- * Physicist's method
--
-- The simplest way to prove such a bound ("physicist's method") is to construct a potential
-- function V, which assigns a nonnegative value to each data structure. If an operation o
-- transforms a structure S into a structure S', then V(S') - V(S), the potential change,
-- is supposed to be a lower estimate on the savings: if a and t are the amortized and actual
-- costs of o, then V(S') - V(S) <= a - t. Thus as you "charge up" the potential, you are
-- accumulating savings from operations that didn't cost as much as the expected bound.
--
-- Suppose in the previous notation that operation o_j transforms structure S_{j-1} into
-- S_j. Then
--
-- sum(t_j : 1 <= j <= k) <= sum(a_j : 1 <= j <= k)
--                           + sum(V(S_{j-1}) - V(S_j) : 1 <= j <= k)
--                         = sum(a_j : 1 <= j <= k) + V(S_0) - V(S_k)
--
-- So to prove the desired amortized estimate is to show that
--
-- sum(t_j : 1 <= j <= k) + V(S_0) - V(S_k) <= sum(a_j : 1 <= j <= k),
--
-- i.e., to show the amortized bound is valid we must show that V(S_k) - V(S_0) >= 0,
-- or in other words that V only increases over any sequence of valid operations.
-- (_Individual_ operations are allowed to discharge potential, to decrease V, as long
-- as they are preceded by operations that have charged up V enough.)
--
-- * Banker's method
--
-- A different way to organize this computation is to associate a "credit" with each location
-- in the structure, and to say that the estimated cost a of an operation is the actual cost t
-- of the operation, plus the credits c it allocates for future use, minus the credits c' it
-- spends to access locations. The credit structure is chosen to satisfy the properties:
--
-- 1. when a credit is allocated on a location, its size is the cost of future access
-- 2. when a credit is spent to access a location, the cost of access is neglected for the
--    spending operation
-- 3. every credit is allocated before it is spent
--
-- For a sequence of operations o_j as above, with this method our estimate is
--
-- a_j = t_j - c_j + c'_j
--
-- Then property 3, that credits are allocated before they are spent, ensures that
--
-- sum(a_j : 1 <= j <= k) =  sum(t_j) - sum(c_j) + sum(c'_j)
--                        >= sum(t_j)                        since sum(c_j) - sum(c'_j) >= 0.
--
-- * Equivalence
--
-- A proof using the banker's method converts to one using the physicist's method by saying the
-- potential V is the sum of accumulated credits so far. Property 3, that credits are
-- allocated before being spent, establishes that V always increases.
--
-- A proof using the physicist's method converts to one "using the banker's method" in a crude
-- way, by putting the potential function as credit on the root. Despite this apparent
-- difference in power, according to the text the methods are provably equivalent.

-- |QUEUES

-- |Exception value representing an empty queue.
data QueueEmpty = QueueEmpty
  deriving (Eq, Show)

-- |Type of queues from Figure 5.1.
class Queue f where

  empty :: f a
  -- ^ An empty queue.

  isEmpty :: f a -> Bool
  -- ^ True if the queue is empty, False otherwise.

  snoc :: f a -> a -> f a
  -- ^ Push an element into the rear of the queue.

  head :: f a -> Either QueueEmpty a
  -- ^ Yield the element at the head of the queue, or QueueEmpty if the queue is empty.

  tail :: f a -> Either QueueEmpty (f a)
  -- ^ Yield the queue with its head element removed, or QueueEmpty if the queue is empty.

-- |Implementation of queues as a pair of lists: BQNonEmpty f r represents a queue
-- containing the elements of f in order, followed by the elements of r in reverse order
-- (so the head of r is the tail of the queue).
--
-- The separation instead of a simple pair of lists as in the text is to make the type
-- enforce the invariant, that the front list is empty only if the entire queue is empty.
--
-- This type also eliminates the auxiliary function `checkf` which served to enforce the
-- invariant; instead `reverse` is called directly in the implementation of `tail`.
data BatchedQueue a =
    BQEmpty
  | BQNonEmpty (NE.NonEmpty a) [a]
  deriving (Eq, Show)

instance Queue BatchedQueue where

  empty = BQEmpty

  -- O(1) worst case since it only examines the data constructor
  isEmpty BQEmpty          = True
  isEmpty (BQNonEmpty _ _) = False

  -- O(1) worst case since it only examines the data constructor and conses
  snoc BQEmpty          x = BQNonEmpty (x :| []) []
  snoc (BQNonEmpty f r) x = BQNonEmpty f            (x:r)

  -- O(1) worst case since it only examines the data constructor and calls the O(1) NE.head
  head BQEmpty          = Left QueueEmpty
  head (BQNonEmpty f _) = Right $ NE.head f

  -- O(n) worst case since it calls reverse, but O(1) amortized, analysis follows.
  tail BQEmpty                        = Left QueueEmpty
  tail (BQNonEmpty (_ :| (x':xs')) r) = Right $ BQNonEmpty (x' :| xs') r
  tail (BQNonEmpty (_ :| []) r)       = Right $ case NE.nonEmpty (reverse r) of
    Nothing -> BQEmpty
    Just r' -> BQNonEmpty r' []

-- |ANALYSIS OF BatchedQueue
--
-- Physicist's method:
--
-- Define the potential function to be the length of the rear list:
-- V(BQEmpty)        = 0
-- V(BQNonEmpty f r) = length r
--
-- Then we estimate the cost of `snoc` as 2:
-- * snoc BQEmpty x costs 1 and increases potential by 0
-- * snoc (BQEmpty f r) x costs 1 and increases potential by 1 (replacing r by x:r)
--
-- And then estimate the amortized cost of `tail` as 1:
-- * the first two cases have worst-case cost 1 and do not change the potential
-- * in the last case, if the potential length r is m, then the execution took m + O(1)
--   steps (m to reverse r) but decreased the potential by m
--
-- Banker's method is similar but put one "credit" on each element of r.
