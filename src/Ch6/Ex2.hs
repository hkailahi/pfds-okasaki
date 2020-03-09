module Ch6.Ex2 where

import BasicPrelude hiding (replicate)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (replicate)

import Ch5.Queue (Queue (snoc))
import Ch6.Classes.BalanceCondition (TwoF_lte_R)
import Ch6.Types.BankersQueue
  (BankersQueue' (front, rear), pattern EmptyBQ, SubQueue (sqElems), prettyBuildBQHistory)

---------------------------------------------------------------------------------------------------

-- Exercise:
-- 1. Change the banker’s queue invariant to 2 * ∣f∣ ≥ |r|. Show that the O(1) amortised bounds
-- still hold.
-- 2. Compare the relative performance of the two implementations on a sequence of one hundred
-- snocs followed by one hundred tails.

---------------------------------------------------------------------------------------------------

newtype LessMovementBQ a = LessMovementBQ
  { unLessMovementBQ :: (BankersQueue' TwoF_lte_R a) }
  deriving stock (Show, Eq, Functor, Foldable)
  deriving Queue via (BankersQueue' TwoF_lte_R)

-- |Building up a `BankersQueue` from a list
buildLMBQ :: [a] -> LessMovementBQ a
buildLMBQ = foldl' snoc (LessMovementBQ EmptyBQ)

-- |Steps of building up a Bankers Queue from a generated list 1..n
buildLMBQHistory :: Int -> [LessMovementBQ Int]
buildLMBQHistory n = scanl' snoc (LessMovementBQ EmptyBQ) [1..n]

-- |Shitty golf begets shitty drawings
prettyBuildLMBQHistory :: Int -> Text
prettyBuildLMBQHistory n =
  printSides $ buildLMBQHistory n
  where
    dupe :: a -> (a, a)
    dupe a = (a, a)
    printElems = tshow . sqElems
    prettySides :: [LessMovementBQ Int] -> [Text]
    prettySides = map
      (uncurry (<>) . bimap
                        ((<>) "Front: " . printElems . front)
                        ((<>) (replicate (max 1 $ n `div` 5) "\t" <> "Rear: ") . printElems . rear)
                   . dupe
                   . unLessMovementBQ)
    printSides :: [LessMovementBQ Int] -> Text
    printSides = intercalate "\n" . prettySides

---------------------------------------------------------------------------------------------------
-- Examples

exBQ :: IO ()
exBQ = putStrLn $ prettyBuildBQHistory 20

exLMBQ :: IO ()
exLMBQ = putStrLn $ prettyBuildLMBQHistory 20

examples :: IO ()
examples = do
  putStrLn "Invariant: |f| >= |r|"
  exBQ
  putStrLn "\n\n"
  putStrLn "Invariant: 2 * |f| >= |r|"
  exLMBQ

{-
1.)
  See https://stappit.github.io/posts/pfds/okasakiPFDSc06.html

  Assume debt function:
    - Assign `D(i) ≤ min(3i, 2 * ∣f∣ − |r|)` to the i-th element of the front

  inv = 2 * ∣f∣ − |r|

  - Non-rotation `snoc` operations
    - Increases `r` by 1, thus decreasing `inv` by 2 (aka 2 * (-r))
      - Can't insert when `D(i) = inv` without rotating (rebalancing/frontloading)
        - So discharge debit built up

  - Non-rotation `tail` operations
    - Two changes
      - Decrease `f` by 1, thus decreasing `inv` by 2 (aka -(2 * f))
        - Can't insert when `D(i) = inv` without rotating (rebalancing/frontloading)
          - So discharge debit built up
      - Decreases the index (`i`) of the remaining nodes by 1, thus decreasing `3i` by 3.
    - Discharging the first three debits in the queue restores the debt invariant.

  - Rotating `snoc` operations
    - (f + r)/2 for frontload

  - Rotating `tail` operations
    - (f + r)/2 for frontload
-}

{-
2.)

Remember that reverse is monolithic.

Since all suspensions are evaluated, the cost of 100 snocs followed by 100 tails is the complete
cost of this sequence of operations. That is, we can pretend that all evaluation is strict.

The only possible difference is in the sum of the lengths of lists that need to be reversed.
- For invariant `|r| ≤ ∣f∣`
  - Cost amounts to `2_0 + 2_1 + ... + 2_5 = 2_6 − 1 = 63`
- For invariant `|r| ≤ 2 * ∣f∣`
  - Cost amounts to `3_0 + 3_1 + 3_2 + 3_3 = 40`
- Thus, we would expect the second invariant to exhibit better performance for the execution
trace above.

-}
