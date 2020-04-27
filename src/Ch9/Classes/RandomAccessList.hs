module Ch9.Classes.RandomAccessList where

import BasicPrelude

-- |Exception type for asking for an element from an empty list.
data Empty = Empty
  deriving (Eq, Show)

-- |Exception type for index out of bounds error
data IndexError = IndexError
  deriving (Eq, Show)

-- |Type class signature for random access lists, from Fig 9.4.
-- f is the type constructor RList from the signature.

class RandomAccessList f where

  empty :: f a
  -- ^ An empty random access list.

  isEmpty :: f a -> Bool
  -- ^ True if the list is empty, False if not.

  cons :: a -> f a -> f a
  -- ^ Yield the second argument with the first argument prepended.

  head :: f a -> Either Empty a
  -- ^ Yield the first element of a list, or Empty if the list is empty.

  tail :: f a -> Either Empty (f a)
  -- ^ Yield the list except the first element, or Empty if the list is empty.

  lookup :: Integer -> f a -> Either IndexError a
  -- ^ Yield the jth element of the list, or IndexError if j < 0 or j >= length of list.

  update :: Integer -> a -> f a -> Either IndexError (f a)
  -- ^ Yield the list with position j replaced by a new object, or IndexError if j < 0 or
  -- j >= length of list.
