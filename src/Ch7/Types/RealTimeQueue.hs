module Ch7.Types.RealTimeQueue where

import BasicPrelude hiding (empty)

import Ch5.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))

data RealTimeQueue a = RealTimeQueue [a] [a] [a]
  deriving (Eq, Show, Functor, Foldable)

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Building up a `ProgressQueue` from a list
buildPQ :: [a] -> RealTimeQueue a
buildPQ = foldl' snoc empty

---------------------------------------------------------------------------------------------------

mkValid :: [a] -> [a] -> [a] -> RealTimeQueue a
mkValid xs ys [] = RealTimeQueue zs [] zs
  where
    zs = rotate xs ys []
mkValid xs ys (_ : zs) = RealTimeQueue xs ys zs

rotate :: [a]
    -> [a]
    -> [a]
    -> [a]
rotate [] [y] zs            = y : zs
rotate (x : xs) (y : ys) zs = x : rotate xs ys (y : zs)
rotate _ _ _                = error "rotate: should not happen"

---------------------------------------------------------------------------------------------------

instance Queue RealTimeQueue where
  empty :: RealTimeQueue a
  empty = RealTimeQueue [] [] []

  isEmpty :: RealTimeQueue a -> Bool
  isEmpty (RealTimeQueue xs _ _) = null xs

  head :: RealTimeQueue a -> Either QueueEmpty a
  head (RealTimeQueue (x:_) _ _) = Right x
  head _                         = Left QueueEmpty

  tail :: RealTimeQueue a -> Either QueueEmpty (RealTimeQueue a)
  tail (RealTimeQueue (_:xs) ys zs) = Right $ mkValid xs ys zs
  tail _                            = Left QueueEmpty

  snoc :: RealTimeQueue a -> a -> RealTimeQueue a
  snoc (RealTimeQueue xs ys zs) x = mkValid xs (x : ys) zs
