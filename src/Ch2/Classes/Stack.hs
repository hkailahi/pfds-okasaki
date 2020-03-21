module Ch2.Classes.Stack where

import BasicPrelude hiding ((++))

data StackEmpty = StackEmpty
  deriving (Show, Eq)

class Stack (s :: * -> *) a where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> Either StackEmpty a
  tail    :: s a -> Either StackEmpty (s a)

---------------------------------------------------------------------------------------

instance Stack [] a where
  empty :: [a]
  empty = []

  isEmpty :: [a] -> Bool
  isEmpty = null

  cons :: a -> [a] -> [a]
  cons = (:)

  head :: [a] -> Either StackEmpty a
  head (x:_) = Right x
  head _     = Left StackEmpty

  tail :: [a] -> Either StackEmpty [a]
  tail (_:xs) = Right xs
  tail _      = Left StackEmpty

  -- |Concatenates two lists
(++) :: [a] -> [a] -> [a]
(++) [] ys     = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- |Updates the i-th element
update :: [a] -> Int -> a -> Either StackEmpty [a]
update []     _  _  = Left StackEmpty
update (_:xs) 0  y  = Right $ y : xs
update (x:xs) ix y  =
  case update xs (ix - 1) y of
    Left  _ -> Left StackEmpty
    Right l -> Right $ x : l

-- TODO instance (Generic s, Generic a) => Stack s a where