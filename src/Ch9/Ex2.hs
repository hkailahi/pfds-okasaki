module Ch9.Ex2 where

import BasicPrelude hiding (empty)

import Ch9.Classes.RandomAccessList

-- Exercise 9.2
-- Write a function create of type `Int -> a -> RList a` that creates a binary random-access list
-- containing n copies of some value x. This function should also run in O(logn) time.
-- (You may find it helpful to review Exercise 2.5.)

naiveInsertN :: (RandomAccessList f) => Int -> a -> f a
naiveInsertN n x =  foldr (\_ acc -> cons x acc) empty [1..n]

{-
λ> naiveInsertN @DenseRAList 20 '!'
DenseRAList
 [ DDZero
 , DDZero
 , DDOne (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!))
 , DDZero
 , DDOne (Nd 16 (Nd 8 (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!))) (Nd 8 (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!))))
 ]

λ> naiveInsertN @SparseRAList 20 '!'
SparseRAList
 [ Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)
 , Nd 16 (Nd 8 (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!))) (Nd 8 (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)))
 ]

λ> naiveInsertN @ZerolessRAList 20 '!'
ZerolessRAList
 [ X Lf! Lf!
 , I (Nd 2 Lf! Lf!)
 , X (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!))
 , I (Nd 8 (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)) (Nd 4 (Nd 2 Lf! Lf!) (Nd 2 Lf! Lf!)))
 ]
-}
