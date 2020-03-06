module Ch6.Ex2 where

import BasicPrelude hiding (replicate)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (replicate)

import Ch5.Queue (Queue (snoc))
import Ch6.Types.BankersQueue
  ( pattern BQ
  , BalanceCondition (needsRebalance)
  , BankersQueue
  , BankersQueue' (BankersQueue, front, rear)
  , pattern EmptyBQ
  , pattern EmptySQ
  , SubQueue (SubQueue, sqElems, sqSize)
  , TwoF_lte_R
  , buildBQ
  , prettyBuildBQHistory
  )

newtype LessMovementBQ a = LessMovementBQ
  { unLessMovementBQ :: (BankersQueue a) }
  deriving stock (Show, Eq, Functor, Foldable)
  deriving Queue via (BankersQueue' TwoF_lte_R)

-- |Building up a `BankersQueue` from a list
buildLMBQ :: [a] -> LessMovementBQ a
buildLMBQ = foldl' snoc (LessMovementBQ EmptyBQ)

-- |Steps of building up a Bankers Queue from a generated list 1..n
buildLMBQHistory :: Int -> [LessMovementBQ Int]
buildLMBQHistory n = scanl' snoc (LessMovementBQ EmptyBQ) [1..n]

-- |Shitty golf begets shitty drawings
prettyBuildLMBQHistory :: Int -> IO ()
prettyBuildLMBQHistory n =
  putStrLn . printSides $ buildLMBQHistory n
  where
    dupe :: a -> (a, a)
    dupe a = (a, a)
    both :: (Bifunctor p) => (a -> b) -> p a a -> p b b
    both f = bimap f f
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



----------------
exBQ :: IO ()
exBQ = prettyBuildBQHistory 15

exLMBQ :: IO ()
exLMBQ = prettyBuildLMBQHistory 15

examples :: IO ()
examples = do
  putStrLn "Invariant: |f| >= |r|"
  exBQ
  putStrLn "\n\n"
  putStrLn "Invariant: 2 * |f| >= |r|"
  exLMBQ

-- Exercise:
-- 1. Change the banker’s queue invariant to 2 * ∣f∣ ≥ |r|. Show that the O(1) amortised bounds still
-- hold.
-- 2. Compare the relative performance of the two implementations on a sequence of one hundred
-- snocs followed by one hundred tails.

{-
1.)
  Assign debt D(i) ≤ min(3i, 2*∣f∣ − |r|) to the ith element of the front

  Every `snoc` that doesn’t cause a rotation increases |r| by 1 and decreases 2∣f∣−|r| by 1. This
  violates the debt invariant by 1 whenever we just previously had D(i)=2∣∣f∣∣−|r|. We can restore
  the invariant by discharging the first debit in the queue, which decreases the rest by 1.

  Every tail that doesn’t cause a rotation dereases ∣f∣ by 1, so decreases 2∣f∣−|r| by 2. It also
  decreases the the index of the remaining nodes by 1, so decreases 3i by 3. Discharging the first
  three debits in the queue restores the debt invariant.

  Now for a snoc that causes a rotation. Just before the rotation, the invariant guarantees that
  all debits in the queue have been discharged, so after the rotation the only undischarged debits
  are those created by the rotation itself. Suppose ∣∣f∣∣=m and |r|=2m+1 at the time of the
  rotation.

  Then we create 2m+1 debits for the reverse and m for the append. The placement of debits is as in
  the book, which is summarised as follows.

  d(i)D(i)=⎧⎩⎨⎪⎪13m+10i<mi=mi>m={i+13m+1i<mi≥m

  The debit invariant is violated at i=0 (since D(0)=1>0) and at i=m (since D(m)=3m+1>3m).
  Discharging one debit from the zeroth node restores the invariant.
-}

{-
2.)
-}
