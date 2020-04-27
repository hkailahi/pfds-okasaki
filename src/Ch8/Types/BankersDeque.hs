module Ch8.Types.BankersDeque where

import BasicPrelude hiding (empty)
import Ch8.Classes.Deque

-- |Banker's deque of Sec 8.4.2. Simple types, nothing shiny here.

data BankersDeque a = BankersDeque { bqFLen  :: Int
                                   , bqFront :: [a]
                                   , bqRLen  :: Int
                                   , bqRear  :: [a]
                                   }

-- |Reverse the roles of the front and rear.
bqFlip :: BankersDeque a -> BankersDeque a
bqFlip BankersDeque{..} = BankersDeque bqRLen bqRear bqFLen bqFront

-- |Answer whether the queue is unbalanced because the front is too long.
-- The loading ratio c is a parameter.
frontTooLong :: Int -> BankersDeque a -> Bool
frontTooLong c BankersDeque{..} = bqFLen > c * bqRLen + 1

-- |Same question in the reverse direction.
rearTooLong :: Int -> BankersDeque a -> Bool
rearTooLong c BankersDeque{..} = bqRLen > c * bqFLen + 1

-- |Carry out the rebalancing transformation by distributing from front to rear.
rebalanceFront :: BankersDeque a -> BankersDeque a
rebalanceFront BankersDeque{..} =
  let totalLen                 = bqFLen + bqRLen
      newFLen                  = totalLen `div` 2
      newRLen                  = totalLen - newFLen
      (newFront, newFirstRear) = splitAt newFLen bqFront
      newRear                  = bqRear ++ reverse newFirstRear
  in BankersDeque newFLen newFront newRLen newRear

-- |Same operation in the reverse direction.
rebalanceRear :: BankersDeque a -> BankersDeque a
rebalanceRear q = bqFlip . rebalanceFront $ bqFlip q

-- |Do all the transformations we might need.
rebalanceIfNeeded :: Int -> BankersDeque a -> BankersDeque a
rebalanceIfNeeded c q
  | frontTooLong c q = rebalanceFront q
  | rearTooLong c q  = rebalanceRear q
  | otherwise        = q

-- |Constant for loading ratio in this implementation because I don't know how to
-- do the necessary type magic.
loadingRatio :: Int
loadingRatio = 2

instance Deque BankersDeque where

  empty = BankersDeque 0 [] 0 []

  isEmpty BankersDeque{..} = bqFLen == 0 && bqRLen == 0

  cons x BankersDeque{..} = rebalanceIfNeeded loadingRatio $
    BankersDeque (bqFLen + 1) (x:bqFront) bqRLen bqRear

  head BankersDeque{..} = case (bqFront, bqRear) of
    (x:_, _)  -> Right x
    ([], [x]) -> Right x
    ([], _:_) -> error "this case is forbidden by balance invariant"
    _         -> Left DequeEmpty

  tail BankersDeque{..} = case (bqFront, bqRear) of
    (_:xs, _) -> Right . rebalanceIfNeeded loadingRatio $
                          BankersDeque (bqFLen - 1) xs bqRLen bqRear
    ([], [_]) -> Right empty
    ([], _:_) -> error "this case is forbidden by balance invariant"
    _         -> Left DequeEmpty

  snoc BankersDeque{..} x = rebalanceIfNeeded loadingRatio $
    BankersDeque bqFLen bqFront (bqRLen + 1) (x:bqRear)

  last BankersDeque{..} = case (bqFront, bqRear) of
    (_, x:_)  -> Right x
    ([x], []) -> Right x
    (_:_, []) -> error "this case is forbidden by balance invariant"
    _         -> Left DequeEmpty

  init BankersDeque{..} = case (bqFront, bqRear) of
    (_, _:xs) -> Right . rebalanceIfNeeded loadingRatio $
                         BankersDeque bqFLen bqFront (bqRLen - 1) xs
    ([_], []) -> Right empty
    (_:_, []) -> error "this case is forbidden by balance invariant"
    _         -> Left DequeEmpty

{- |ANALYSIS of BankersDeque (Ex 8.5)

As with the earlier queue, define the debit accounting structure as follows:

d+(i) is the number of debits on the ith node of the front stream
D+(i) = sum(d+(j) : 0 <= j <= i)
d-(i) and D-(i) are defined analogously for the rear stream

Claim that the following invariant holds: for both D+ and D-,

D(i) <= min{ (c + 1) i, cs - t + 1 }  where s = min{flen, rlen}, t = max{flen, rlen}.

Because D(i) <= (c + 1) i, the 0th node of each stream always has 0 debits, and is accessible.
Because D(i) <= cs - t + 1, whenever rebalance is triggered (t > cs + 1) the entire structure
has 0 debits.

Consing or snocing onto the shorter stream of the two loosens both branches of the debit
invariant: the index of each node in the augmented stream goes up, so (c + 1) i increases by
c + 1 at each node; and s goes up, so cs - t + 1 increases by c at each node.

Consing or snocing onto the longer or equal stream of the two loosens the first branch of
the debit invariant, but tightens the second by one; so this is OK if it pays the first debit.
Rebalancing if needed is then allowed by the earlier claim.

So the cost of cons or snoc is constant (to execute) + 1 debit, still O(1).

Tailing or initing from either stream tightens the first branch of the debit invariant by c + 1,
since every element in that stream goes down 1 in index; tailing or initing from the shorter
stream in addition tightens the second branch by c. This is OK if we pay c + 1 debits.
Rebalancing if needed is then allowed by the earlier claim.

So the cost of tail or init is constant (to execute) + c + 1 debits, still O(1).

Finally, to pay for the accesses involved in rebalancing:  Suppose without loss of generality
that the front stream was the longer one; then

* forcing each element of newFront costs 1 over the previous, so we put 1 debit on the first
  newFLen elements of the new front;
* reversing newFirstRear to stick it on the end of newRear costs FLen - newFLen operations
  all at once, so we put all FLen - newFLen debits on node RLen of the new rear (the first
  node of reverse newFirstRear).

After this:

* if we put S = min{newFLen, newRLen} = newFLen, T = max{newFLen, newRLen} = newRLen (this
  is true because of the way rebalance chooses where the odd element goes) then either S = T
  or S + 1 = T. Hence, as c >= 2, cS - T + 1 >= S, and therefore the new front, with 1 debit
  on each of its S nodes, obeys the debit invariant.

* For the rear, we have D-(i) = 0 for 0 <= i < RLen, and D-(i) = FLen - newFLen for i >= RLen.
  The total length at rebalance is s + cs + 1 (the 1 triggers rebalance), FLen = cs + 1,
  newFLen = ((c + 1)/2) s, so D-(i) <= ((c - 1)/2)RLen + 1, and the first branch of the debit
  invariant is still satisfied; the second branch is D(i) <= cS - T + 1 <= ((c - 1)/2) s,
  and to satisfy the second we need to pay at most one more debit.

Thus rebalancing requires us to add at most one more debit payoff to any operation, which
completes the proof that all operations are O(1).

-}

{- |Relative performance due to rebalancing factors (Ex 8.6)

The cost of rebalancing is FLen, the length of the old front list. Generally this will be
c/(c + 1) of the cost of the queue, so 2/3 for c = 2 and 4/5 for c = 4. On the other hand,
after a rebalance, suppose we are inserting only at one end; then another rebalance will
when the total queue size exceeds (c + 1)/2 of the size beforehand (the added mass is
(c - 1) times the mass of each side before). This is 3/2 for c = 2 and 5/2 for c = 4.

Thus, if we are inserting continuously on one side, rebalances happen when

log n / log((c + 1)/2)

passes an integer, and each rebalance costs c / (c + 1) n. 

This means that operations continuously inserting or removing from one side are much more
expensive with c = 2 than with c = 4, since the difference in frequency is exponential in
something proportional to c, and the difference in cost is only proportional to c.

Conversely, if only a constant number of rebalances happen, then the rebalancing is more
expensive with c = 4 than with c = 2.

-}
