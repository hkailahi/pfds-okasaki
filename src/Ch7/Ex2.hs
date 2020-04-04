module Ch7.Ex2 where

import BasicPrelude
import qualified StrictList as SL

import Ch7.Types.RealTimeQueue (RealTimeQueue (RealTimeQueue))

-- Exercise 7.2
-- Compute the size of a queue from the sizes of `s` and `r`. How much faster might such a
-- function run than one that measures the sizes of `f` and `r`?

-- |Compute size of RTQ from rear and schedule
sizeFromRS :: RealTimeQueue a -> Int
sizeFromRS (RealTimeQueue _ SL.Nil acc) = length acc
sizeFromRS (RealTimeQueue _ rear   acc) = 2 * length rear + length acc

-- |Compute size of RTQ from rear and schedule
sizeFromFR :: RealTimeQueue a -> Int
sizeFromFR (RealTimeQueue front rear _) = length front + length rear

{-
|r| = 0   ==> |f| = |s| ==> |r| + |s| =  |f| + |r|
|s| = 0   ==> |r| = |f| ==> |r| + |s| = (|f| + |r|)/2

Taking the size of rear is equivalent in both cases, and doesn't force any
thunks in the front or schedule.

The schedule can between 0 and |r| smaller on empty case, so evaluting schedule
is faster.

Therefore, it becomes 1-2 times faster.
-}
