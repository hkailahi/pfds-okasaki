module Ch7.Ex3 where

import BasicPrelude ()

import Ch7.Types.ProgressQueue ()

-- Exercise 7.3
-- Show that it does no harm to the running time of insert to remove the lazy annotation from
-- the definition of `insTree`.

{-
|r| = 0   ==> |f| = |s| ==> |r| + |s| =  |f| + |r|
|r| = |f| ==> |s| = 0   ==> |r| + |s| = (|f| + |r|)/2

Therefore, it becomes 1-2 times faster.
-}
