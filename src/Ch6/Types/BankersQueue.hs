module Ch6.Types.BankersQueue where

import BasicPrelude hiding (replicate)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (replicate)

import Ch5.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))

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
data BankersQueue a = BankersQueue
  { front :: SubQueue a
  , rear  :: SubQueue a
  } deriving (Show, Eq, Functor, Foldable)

{-# COMPLETE BQ #-}
-- |Because I wanted named fields, but actually requiring field names is annoying because in most
-- cases they're just noise. With explicit exports, I can decide to not export this pattern if I'm
-- worried about user's of my `BankersQueue` libary misusing it.
pattern BQ :: Int -> [a] -> Int -> [a] -> BankersQueue a
pattern BQ sizeF front sizeR rear = BankersQueue
  { front = SubQueue sizeF front
  , rear  = SubQueue sizeR rear
  }

pattern EmptyBQ :: BankersQueue a
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

-- |"check" in book
-- Makes sure front is always larger than rear
frontLoad :: BankersQueue a -> BankersQueue a
frontLoad q@(BQ lenF f lenR r)
  | lenF >= lenR = q
  | otherwise   = BankersQueue
    { front = SubQueue { sqSize = lenF + lenR, sqElems = f ++ reverse r }
    , rear  = EmptySQ
    }
  -- -- | lenR <= lenF = q
  -- | lenF < lenR  = q
  -- | lenF >= lenR = BankersQueue
  --   { front = SubQueue { sqSize = lenF + lenR, sqElems = f ++ reverse r }
  --   , rear  = EmptySQ
  --   }

---------------------------------------------------------------------------------------------------

instance Queue BankersQueue where
  -- |O(1)
  empty :: BankersQueue a
  empty = EmptyBQ

  -- |O(1)
  isEmpty :: BankersQueue a -> Bool
  isEmpty (BQ 0 _ _ _) = True  -- ^Not matching on EmptyBQ since either size or lists are unused
  isEmpty _            = False

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  snoc :: BankersQueue a -> a -> BankersQueue a
  snoc (BQ lenF f lenR r) x = frontLoad $ BQ lenF f (lenR + 1) (x:r)

  -- |O(1)
  head :: BankersQueue a -> Either QueueEmpty a
  head (BQ _ (x:_) _ _) = Right x
  head _                = Left QueueEmpty

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  tail :: BankersQueue a -> Either QueueEmpty (BankersQueue a)
  tail (BQ lenF (_:xs) lenR r) = Right . frontLoad $ BQ (lenF - 1) xs lenR r
  tail _                       = Left QueueEmpty
