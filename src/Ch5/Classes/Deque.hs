module Ch5.Classes.Deque where

import BasicPrelude

-- |Exception value representing an empty deque.
data DequeEmpty = DequeEmpty
  deriving (Eq, Show)

-- |Type of deques from Figure 5.3.
class Deque f where

  empty :: f a
  -- ^ An empty deque.

  isEmpty :: f a -> Bool
  -- ^ True if the deque is empty, False otherwise.

  cons :: a -> f a -> f a
  -- ^ Insert at head.

  head :: f a -> Either DequeEmpty a
  -- ^ Yield element at head, or DequeEmpty if deque is empty.

  tail :: f a -> Either DequeEmpty (f a)
  -- ^ Yield deque with head element removed, or DequeEmpty if deque is empty.

  snoc :: f a -> a -> f a
  -- ^ Insert at tail.

  last :: f a -> Either DequeEmpty a
  -- ^ Yield element at tail, or DequeEmpty if deque is empty.

  init :: f a -> Either DequeEmpty (f a)
  -- ^ Yield deque with tail element removed, or DequeEmpty if deque is empty.
