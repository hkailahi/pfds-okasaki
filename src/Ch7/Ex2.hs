module Ch7.Ex2 where

import BasicPrelude ()

import Ch7.Types.ProgressQueue ()

-- Exercise 7.2
-- Compute the size of a queue from the sizes of `s` and `r`. How much faster might such a
-- function run than one that measures the sizes of `f` and `r`?

{-
|r| = 0   ==> |f| = |s| ==> |r| + |s| =  |f| + |r|
|r| = |f| ==> |s| = 0   ==> |r| + |s| = (|f| + |r|)/2

Therefore, it becomes 1-2 times faster.
-}
