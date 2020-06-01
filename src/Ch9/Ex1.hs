module Ch9.Ex1 where

import BasicPrelude hiding (tail)

import Ch9.Classes.RandomAccessList
import qualified Ch9.Types.DenseRandomAccessList as DRAL

-- Exercise 9.1
-- Write a function drop of type `Int -> RList a -> RList a` that deletes the first k elements of
-- a binary random-access list. Your function should run in O(logn) time

naiveDropK :: (RandomAccessList f) => Int -> f a -> f a
naiveDropK n ral =
  foldr (\_ acc -> case tail acc of Right t -> t; Left e -> error $ show e) ral [1..n]

-- λ> naiveDropK @SparseRAList 5 $ demoCons 10
-- SparseRAList [CBLeaf '!',CBNode 4 (CBNode 2 (CBLeaf '!') (CBLeaf '!')) (CBNode 2 (CBLeaf '!') (CBLeaf '!'))]
-- λ> naiveDropK @SparseRAList 3 $ demoCons 10
-- SparseRAList [CBLeaf '!',CBNode 2 (CBLeaf '!') (CBLeaf '!'),CBNode 4 (CBNode 2 (CBLeaf '!') (CBLeaf '!')) (CBNode 2 (CBLeaf '!') (CBLeaf '!'))]

drop :: Integer -> DRAL.DenseRAList a -> Either IndexError (DRAL.DenseRAList a)
drop = DRAL.drop
