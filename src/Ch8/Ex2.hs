module Ch8.Ex2 where

import BasicPrelude hiding (empty, tail)

import Ch5.Classes.Queue (Queue (empty, snoc, tail))
import Ch8.Types.HoodMelvilleQueue (HoodMelvilleQueue)

---------------------------------------------------------------------------------------------------

-- Exercise 8.2
-- Prove that calling `exec` twice at the beginning of each rotation, and once for every remaining
-- insertion or deletion is enough to finish the rotation on time. Modify the code accordingly.

-- Answer: https://github.com/rst76/pfds/blob/09348ea4838eadf263e23f08585e4fce6d5fb49f/ch08/ex.8.2.hs

---------------------------------------------------------------------------------------------------

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right b) = b
unsafeFromRight _         = error "ux2ux"

main :: IO ()
main = do
  let qs :: [HoodMelvilleQueue Int]
      qs = scanl snoc empty [1 .. 7]
  mapM_ print qs
  -- => HM 0 [] Idle 0 []
  -- => HM 1 [1] Idle 0 []
  -- => HM 1 [1] Idle 1 [2]
  -- => HM 3 [1] (Appending 1 [1] [2,3]) 0 []
  -- => HM 3 [1] (Appending 0 [] [1,2,3]) 1 [4]
  -- => HM 3 [1,2,3] Idle 2 [5,4]
  -- => HM 3 [1,2,3] Idle 3 [6,5,4]
  -- => HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) 0 []
  mapM_ print . take 8 . iterate (unsafeFromRight . tail) $ last qs
  -- => HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) 0 []
  -- => HM 6 [2,3] (Reversing 2 [] [3,2,1] [4] [5,6,7]) 0 []
  -- => HM 5 [3] (Appending 1 [3,2,1] [4,5,6,7]) 0 []
  -- => HM 4 [4,5,6,7] Idle 0 []
  -- => HM 3 [5,6,7] Idle 0 []
  -- => HM 2 [6,7] Idle 0 []
  -- => HM 1 [7] Idle 0 []
  -- => HM 0 [] Idle 0 []
