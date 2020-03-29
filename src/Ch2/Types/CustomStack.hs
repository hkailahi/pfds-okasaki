module Ch2.Types.CustomStack where

import BasicPrelude hiding ((++))

import Ch2.Classes.Stack (Stack (cons, empty, head, isEmpty, tail), StackEmpty (StackEmpty))

---------------------------------------------------------------------------------------

data CustomStack a =
    Nil
  | Cons a (CustomStack a)
  deriving (Show, Eq)

instance Stack CustomStack where
  empty :: CustomStack a
  empty = Nil

  isEmpty :: CustomStack a -> Bool
  isEmpty Nil = True
  isEmpty _   = False

  cons :: a -> CustomStack a -> CustomStack a
  cons = Cons

  head :: CustomStack a -> Either StackEmpty a
  head (Cons x _) = Right x
  head _          = Left StackEmpty

  tail :: CustomStack a -> Either StackEmpty (CustomStack a)
  tail (Cons _ xs) = Right xs
  tail _           = Left StackEmpty
