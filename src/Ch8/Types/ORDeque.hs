module Ch8.Types.ORDeque where

import BasicPrelude
import Ch8.Classes.Deque (DequeEmpty (DequeEmpty))
import Ch8.Classes.ORDeque
import qualified Ch8.Classes.Queue as Q

-- |Exercise 8.4: Trivial implementation of an output-restricted deque based on an existing queue.

-- |Wrap a queue in a thing that attaches a list to it.
data HeadListORDeque q a = HeadListORDeque [a] (q a)

instance (Q.Queue q) => ORDeque (HeadListORDeque q) where

  empty = HeadListORDeque [] Q.empty

  isEmpty (HeadListORDeque front rest) = null front && Q.isEmpty rest

  cons x (HeadListORDeque front rest) = HeadListORDeque (x:front) rest

  head = \ case
    HeadListORDeque (x:_) _ -> Right x
    HeadListORDeque []    rest -> case Q.head rest of
      Left Q.QueueEmpty -> Left DequeEmpty
      Right x           -> Right x

  tail = \ case
    HeadListORDeque (_:xs) rest -> Right $ HeadListORDeque xs rest
    HeadListORDeque []     rest -> case Q.tail rest of
      Left Q.QueueEmpty -> Left DequeEmpty
      Right rest'       -> Right $ HeadListORDeque [] rest'

  snoc (HeadListORDeque front rest) x = HeadListORDeque front $ Q.snoc rest x

