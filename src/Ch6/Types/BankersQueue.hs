{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ch6.Types.BankersQueue where

import BasicPrelude hiding (replicate)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (replicate)

import Ch5.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))

---------------------------------------------------------------------------------------------------
-- Parematerizing the balance invariant

class BalanceCondition c where
  needsRebalance :: Int -> Int -> Bool

data F_lte_R
data TwoF_lte_R

instance BalanceCondition F_lte_R where
  needsRebalance :: Int -> Int -> Bool
  needsRebalance lenF lenR = not $ lenF >= lenR

instance BalanceCondition TwoF_lte_R where
  needsRebalance :: Int -> Int -> Bool
  needsRebalance lenF lenR = not $ 2 * lenF >= lenR

---------------------------------------------------------------------------------------------------
-- Types and Accessors

-- |Front + Rear (or Left + right) `SubQueue`s make a `BankersQueue`
data SubQueue a = SubQueue
  { sqSize  :: Int
  , sqElems :: [a]
  } deriving (Show, Eq, Functor, Foldable)

pattern EmptySQ :: SubQueue a
pattern EmptySQ = SubQueue 0 []

-- |Allows reads and writes to both ends of the queue. The invariant is updated to be symmetric
-- in its treatment of f and r: both are required to be non-empty whenever the deque contains
-- two or more elements. When one list becomes empty, we split the other list in half and
-- reverse one of the halves.
--
-- Laws:
--   - |front| >= |rear|
--    - Thus front is empty only if rear is also empty
data BankersQueue' invariant a = BankersQueue
  { front :: SubQueue a
  , rear  :: SubQueue a
  } deriving (Show, Eq, Functor, Foldable)
type BankersQueue a = BankersQueue' F_lte_R a

{-# COMPLETE BQ #-}
-- |Because I wanted named fields, but actually requiring field names is annoying because in most
-- cases they're just noise. With explicit exports, I can decide to not export this pattern if I'm
-- worried about user's of my `BankersQueue` libary misusing it.
pattern BQ :: Int -> [a] -> Int -> [a] -> BankersQueue' inv a
pattern BQ sizeF front sizeR rear = BankersQueue
  { front = SubQueue sizeF front
  , rear  = SubQueue sizeR rear
  }

pattern EmptyBQ :: BankersQueue' inv a
pattern EmptyBQ = BankersQueue
  { front = EmptySQ
  , rear  = EmptySQ
  }

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Building up a `BankersQueue` from a list
buildBQ :: [a] -> BankersQueue a
buildBQ = foldl' snoc EmptyBQ

-- |Steps of building up a Bankers Queue from a generated list 1..n
buildBQHistory :: Int -> [BankersQueue Int]
buildBQHistory n = scanl' snoc EmptyBQ [1..n]

-- |Shitty golf begets shitty drawings
prettyBuildBQHistory :: Int -> IO ()
prettyBuildBQHistory n =
  putStrLn . printSides $ buildBQHistory n
  where
    dupe :: a -> (a, a)
    dupe a = (a, a)
    both :: (Bifunctor p) => (a -> b) -> p a a -> p b b
    both f = bimap f f
    printElems = tshow . sqElems
    prettySides :: [BankersQueue Int] -> [Text]
    prettySides = map
      (uncurry (<>) . bimap
                        ((<>) "Front: " . printElems . front)
                        ((<>) (replicate (max 1 $ n `div` 5) "\t" <> "Rear: ") . printElems . rear)
                   . dupe)
    printSides :: [BankersQueue Int] -> Text
    printSides = intercalate "\n" . prettySides


mkBQ :: [a] -> [a] -> BankersQueue a
mkBQ f r = BQ (length r) r (length f) f

---------------------------------------------------------------------------------------------------
-- Parameterized "check" function so we can play with invariant

-- |"check" in book
-- Makes sure front is always larger than rear
frontLoad ::
  forall invariant a. (BalanceCondition invariant) =>
  BankersQueue' invariant a -> BankersQueue' invariant a
frontLoad q@(BQ lenF f lenR r)
  | needsRebalance @invariant lenF lenR = BankersQueue
      { front = SubQueue { sqSize = lenF + lenR, sqElems = f ++ reverse r }
      , rear  = EmptySQ
      }
  | otherwise                           = q

---------------------------------------------------------------------------------------------------

instance (BalanceCondition invariant) => Queue (BankersQueue' invariant) where
  -- |O(1)
  empty :: BankersQueue' invariant a
  empty = EmptyBQ

  -- |O(1)
  isEmpty :: BankersQueue' invariant a -> Bool
  isEmpty (BQ 0 _ _ _) = True  -- ^Not matching on EmptyBQ since either size or lists are unused
  isEmpty _            = False

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  snoc :: (BalanceCondition invariant)
    => BankersQueue' invariant a
    -> a
    -> BankersQueue' invariant a
  snoc q@(BQ lenF f lenR r) x = frontLoad $ BQ lenF f (lenR + 1) (x:r)
  -- snoc q@(BQ lenF f lenR r) x
    -- | needsRebalance @invariant lenF lenR = frontLoad $ BQ lenF f (lenR + 1) (x:r)
    -- | otherwise                           = q

  -- |O(1)
  head :: BankersQueue' invariant a -> Either QueueEmpty a
  head (BQ _ (x:_) _ _) = Right x
  head _                = Left QueueEmpty

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  tail :: forall invariant a. (BalanceCondition invariant)
   => BankersQueue' invariant a
   -> Either QueueEmpty (BankersQueue' invariant a)
  tail q@(BQ lenF (_:xs) lenR r) = Right . frontLoad $ BQ (lenF - 1) xs lenR r
  -- tail q@(BQ lenF (_:xs) lenR r)
  --   | needsRebalance @invariant lenF lenR = Right . frontLoad $ BQ (lenF - 1) xs lenR r
    -- | otherwise                           = Right q
  tail _                         = Left QueueEmpty
