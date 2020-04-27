module Ch8.Classes.ORDeque where

import BasicPrelude

import Ch8.Classes.Deque (DequeEmpty)

-- |OUTPUT-RESTRICTED DEQUES

class ORDeque f where

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
