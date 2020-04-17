module Ch3.Ex9 where

import BasicPrelude hiding (Set, insert, empty, delete)

import Ch2.Classes.Set (Set (empty, insert))
import Ch3.Types.RBT (RedBlackSet)

buildSet :: (Set s a) => [a] -> s a
buildSet = foldl' (flip insert) empty -- equiv to foldr insert E . reverse

fromOrdList :: (Set s a) => [a] -> s a
fromOrdList = buildSet

buildRedBlackSet :: Ord a => [a] -> RedBlackSet a
buildRedBlackSet = fromOrdList @RedBlackSet
