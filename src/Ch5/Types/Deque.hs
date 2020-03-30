module Ch5.Types.Deque where

import BasicPrelude
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

import Ch5.Classes.Deque
  (Deque (cons, empty, head, init, isEmpty, last, snoc, tail), DequeEmpty (DequeEmpty))

-- |EXERCISE 5.1

-- |Similarly to the BatchedQueue type, we implement BatchedDeque using two lists, of which
-- either both are empty, the front contains exactly one element, or both are nonempty.
data BatchedDeque a =
    BDZero
  | BDOne a
  | BDMore (NE.NonEmpty a) (NE.NonEmpty a)
  deriving (Eq, Show)

-- |Reverse the back half of a list and return it separately. Leave an odd element on the
-- reversed half. This is a helper function for the implementation of BatchedDeque.
-- (The choice of where to leave the odd element is significant.)
--
-- The contract is that
--   reverseHalf xs = (ys, zs)
--   <==> ys ++ (reverse zs) = xs -and- 0 <= length ys - length zs <= 1.
--
-- >>> reverseHalf []
-- ([], [])
--
-- >>> reverseHalf [1]
-- ([], [1])
--
-- >>> reverseHalf [1, 2, 3, 4]
-- ([1, 2], [4, 3])
--
-- >>> reverseHalf [1, 2, 3, 4, 5]
-- ([1, 2], [5, 4, 3])
--
-- This could be done with a single traversal with a partial function based cheat or smarter
-- types, but it's not really essential to the point of the problem. The implementation given
-- takes time 3 * length of argument where 3 is a global constant, which is what matters.
reverseHalf :: [a] -> ([a], [a])
reverseHalf xs =
  let (front, back) = splitAt (length xs `div` 2) xs
  in (front, reverse back)

instance Deque BatchedDeque where

  empty = BDZero

  -- O(1) worst case
  isEmpty BDZero = True
  isEmpty _      = False

  -- O(1) worst case
  cons x BDZero         = BDOne x
  cons x (BDOne y)      = BDMore (x :| []) (y :| [])
  cons x (BDMore ys zs) = BDMore (x <| ys) zs

  -- O(1) worst case
  head BDZero              = Left DequeEmpty
  head (BDOne y)           = Right y
  head (BDMore (y :| _) _) = Right y

  -- O(n) worst case, but O(1) amortized, analysis below
  tail BDZero                       = Left DequeEmpty
  tail (BDOne _)                    = Right BDZero
  tail (BDMore (_ :| (y':ys')) zs)  = Right $ BDMore (y' :| ys') zs
  tail (BDMore (_ :| []) (z :| zs)) = Right $
    let (stillBack, nowFront) = reverseHalf zs
    in case NE.nonEmpty nowFront of
      -- if nowFront is empty, so is stillBack
      Nothing  -> BDOne z
      -- note that length stillBack <= length nowFront <= length stillBack + 1
      Just ys' -> BDMore ys' (z :| stillBack)

  -- O(1) worst case
  snoc BDZero         x = BDOne x
  snoc (BDOne y)      x = BDMore (y :| []) (x :| [])
  snoc (BDMore ys zs) x = BDMore ys        (x <| zs)

  -- O(1) worst case
  last BDZero              = Left DequeEmpty
  last (BDOne y)           = Right y
  last (BDMore _ (z :| _)) = Right z

  -- O(n) worst case, but O(1) amortized, analysis below
  init BDZero                       = Left DequeEmpty
  init (BDOne _)                    = Right BDZero
  init (BDMore ys (_ :| (z':zs')))  = Right $ BDMore ys (z' :| zs')
  init (BDMore (y :| ys) (_ :| [])) = Right $
    let (stillFront, nowBack) = reverseHalf ys
    in case NE.nonEmpty nowBack of
      -- if nowBack is empty, so is stillFront
      Nothing  -> BDOne y
      -- note that length stillFront <= length nowBack <= length stillFront + 1
      Just zs' -> BDMore (y :| stillFront) zs'

-- |ANALYSIS OF BatchedDeque
--
-- Define the potential V to be 3 times the absolute difference between the lengths of the
-- front and rear lists:
-- V(BDZero)       = 0
-- V(BDOne _)      = 3
-- V(BDMore xs ys) = 3 * abs (length xs - length ys)
--
-- Then we estimate the cost of `cons` and `snoc` by 4. Unlike the case of queues, this is an
-- explicit overestimate in cases besides the base case:
-- * cons x BDZero costs 1 and increases potential by 3
-- * cons x (BDOne y) costs 1 and _decreases_ potential by 3, for an actual amortized cost of
--   0, but we still assign it a cost estimate of 4
-- * cons x (BDMore ys zs) costs 1 and may increase or decrease potential by 3 depending on
--   the relative lengths of ys and zs; it doesn't really matter
--
-- For `tail`, we can estimate the amortized cost by 6 + O(1), as follows:
-- * tail BDZero has worst-case cost 1 and does not change the potential
-- * tail (BDOne y) has worst-case cost 1 and _decreases_ the potential by 3
-- * tail (BDMore (y :| (y':ys')) zs) has worst-case cost 1, and may increase or decrease
--   potential by 3, according as the front is shorter or longer than the back
-- * The last case can only be called when the potential is already 3 * length zs, and
--   it costs O(1) + 3 * (length zs + 1) but reduces the potential to at most 3.  So all
--   but 6 + O(1) of the cost are absorbed in discharging the potential.
-- The case of `init` is analogous.

