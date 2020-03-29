module Ch7.Types.ProgressQueue where

import BasicPrelude

import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.STRef.Lazy (STRef, newSTRef, readSTRef, writeSTRef)

import Ch5.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))
import Ch6.Classes.BalanceCondition (BalanceCondition (needsRebalance), F_lte_R)

-- The following implementation is based on https://www.well-typed.com/blog/2016/01/efficient-queues/

---------------------------------------------------------------------------------------------------

data StrictList a =
    SNil
  | SCons a !(StrictList a)
  deriving (Show, Eq, Functor, Foldable)

---------------------------------------------------------------------------------------------------

data Progress =
    Done
  | NotYet Progress
  deriving (Show, Eq)

step :: Progress -> Progress
step Done       = Done
step (NotYet p) = p

forceSpine :: Int -> [a] -> Progress
forceSpine 0 _      = Done
forceSpine _ []     = Done
forceSpine n (_:xs) = NotYet (forceSpine (n-1) xs)

par :: Progress -> Progress -> Progress
par !p         Done        = p
par Done       !p'         = p'
par (NotYet p) (NotYet p') = NotYet (par p p')

data Delay a =
    Now a
  | Later (Delay a)
  deriving (Show, Eq)

revDelay :: StrictList a -> Delay [a]
revDelay = go []
  where
    go :: [a] -> StrictList a -> Delay [a]
    go acc SNil         = Now acc
    go acc (SCons x xs) = Later $ go (x:acc) xs

runDelay :: Delay a -> (a, Progress)
runDelay = \xs -> runST $ do
    r <- newSTRef xs
    x <- unsafeInterleaveST $ readSTRef r
    p <- next r
    return (runNow x, p)
  where
    next :: STRef s (Delay a) -> ST s Progress
    next r = do
      xs <- readSTRef r
      case xs of
        Now _   -> return Done
        Later d -> do writeSTRef r d
                      p' <- next r
                      return $ NotYet p'
    runNow :: Delay a -> a
    runNow (Now   a) = a
    runNow (Later d) = runNow d

---------------------------------------------------------------------------------------------------

data LazySubQueue a = LazySubQueue
  { lsqSize  :: !Int
  , lsqElems :: [a]
  }
  deriving (Show, Eq, Functor, Foldable)

pattern EmptyLSQ :: LazySubQueue a
pattern EmptyLSQ = LazySubQueue 0 []

data StrictSubQueue a = StrictSubQueue
  { ssqSize  :: !Int
  , ssqElems :: !(StrictList a)
  }
  deriving (Show, Eq, Functor, Foldable)

pattern EmptySSQ :: StrictSubQueue a
pattern EmptySSQ = StrictSubQueue 0 SNil

-- | Todo some description
-- Read: `ProgressQueue !Int [a] !Int !(StrictList a) !Progress`
data ProgressQueue' invariant a = ProgressQueue
  { front    :: !(LazySubQueue a)
  , rear     :: !(StrictSubQueue a)
  , progress :: !Progress
  }
  deriving (Show, Eq, Functor, Foldable)
type role ProgressQueue' nominal representational
type ProgressQueue a = ProgressQueue' F_lte_R a

{-# COMPLETE PQ #-}
pattern PQ :: Int -> [a] -> Int -> StrictList a -> Progress ->  ProgressQueue' inv a
pattern PQ sizeF front sizeR rear prg = ProgressQueue
  { front    = LazySubQueue sizeF front
  , rear     = StrictSubQueue sizeR rear
  , progress = prg
  }

pattern EmptyPQ :: ProgressQueue' inv a
pattern EmptyPQ = ProgressQueue
  { front    = EmptyLSQ
  , rear     = EmptySSQ
  , progress = Done
  }

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Building up a `ProgressQueue` from a list
buildPQ :: [a] -> ProgressQueue a
buildPQ = foldl' snoc EmptyPQ

---------------------------------------------------------------------------------------------------

progInvariant :: forall invariant a. (BalanceCondition invariant)
  => ProgressQueue' invariant a -> ProgressQueue' invariant a
progInvariant q@(PQ lenF xs lenR ys _)
  | needsRebalance @invariant lenF lenR  = let (ys', p1) = runDelay $ revDelay ys
                                               xs'       = xs ++ ys'
                                               p2        = forceSpine lenF xs'
                                           in PQ (lenF + lenR) xs' 0 SNil (par p1 p2)
  | otherwise                            = q

instance (BalanceCondition invariant) => Queue (ProgressQueue' invariant) where
  -- |O(1)
  empty :: ProgressQueue' invariant a
  empty = EmptyPQ

  -- |O(1)
  isEmpty :: ProgressQueue' invariant a -> Bool
  isEmpty (PQ 0 _ _ _ _) = True  -- ^Not matching on EmptyBQ since either size or lists are unused
  isEmpty _              = False

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  snoc :: ProgressQueue' invariant a -> a -> ProgressQueue' invariant a
  snoc (PQ lenF f lenR r prog) x = progInvariant $ PQ lenF f (lenR + 1) (SCons x r) (step prog)

  -- |O(1)
  head :: ProgressQueue' invariant a -> Either QueueEmpty a
  head (PQ _ (x:_) _ _ _) = Right x
  head _                  = Left QueueEmpty

--   head (Q3 _ (x:_ ) _ _  _)   = x
--   tail (Q3 f (_:xs) r ys p)   = inv3 $ Q3 (f-1) xs r ys (step p)
--   snoc (Q3 f xs     r ys p) y = inv3 $ Q3 f xs (r+1) (SCons y ys) (step p)

  -- |Amortized O(1)
  -- Only when front is smaller than rear does the O(r) frontload happen
  tail :: ProgressQueue' invariant a -> Either QueueEmpty (ProgressQueue' invariant a)
  tail (PQ lenF (_:xs) lenR r prog) = Right . progInvariant $ PQ (lenF - 1) xs lenR r (step prog)
  tail _                            = Left QueueEmpty
