module Ch8.ReverseExample where

import BasicPrelude

-- A list that gets incrementally reversed
data ReverseState a = 
    Working [a] [a] -- ^A pair of unreversed and reversed parts of a list
  | Done [a]        -- ^A fully reversed list
  deriving (Show, Eq)

-- |Initialize rotation
startReverse :: [a] -> ReverseState a
startReverse xs = Working xs []

-- |@exec@ in book
-- Rotates a single element. Call n + 1 times on freshly initialized ReverseState to fully reverse
-- list.
rotate :: ReverseState a -> ReverseState a
rotate (Working (x:xs) rev) = Working xs (x:rev)
rotate (Working []     rev) = Done rev
rotate (Done rev)           = Done rev
