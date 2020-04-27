module Ch8.Classes.Queue where

import BasicPrelude

-- |QUEUES

-- |Exception value representing an empty queue.
data QueueEmpty = QueueEmpty
  deriving (Eq, Show)

-- |Type of queues from Figure 5.1.
class Queue f where

  empty :: f a
  -- ^ An empty queue.

  isEmpty :: f a -> Bool
  -- ^ True if the queue is empty, False otherwise.

  snoc :: f a -> a -> f a
  -- ^ Push an element into the rear of the queue.

  head :: f a -> Either QueueEmpty a
  -- ^ Yield the element at the head of the queue, or QueueEmpty if the queue is empty.

  tail :: f a -> Either QueueEmpty (f a)
  -- ^ Yield the queue with its head element removed, or QueueEmpty if the queue is empty.
