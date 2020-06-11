module Ch10.Classes.CatenableList where

import BasicPrelude hiding (head, tail, (++))

data CatenableListEmpty = CatenableListEmpty deriving (Show, Eq)

class CatenableList c where
  empty   :: c a
  isEmpty :: c a -> Bool

  cons :: a -> c a -> c a
  snoc :: c a -> a -> c a

  (++) :: c a -> c a -> c a

  head :: c a -> Either CatenableListEmpty a
  tail :: c a -> Either CatenableListEmpty (c a)
