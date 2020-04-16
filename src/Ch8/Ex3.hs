module Ch8.Ex3 where

import BasicPrelude hiding (empty)
import Control.Lens (each, toListOf)
import qualified Prelude as P
import Text.Layout.Table
  (ColSpec, center, column, def, dotAlign, fixed, rowG, tableString, titlesH, unicodeS)

import Ch5.Classes.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))
  
---------------------------------------------------------------------------------------------------

-- Exercise 8.3
-- Replace the @lenf@ and @lenr@ fields with a single @diff@ field that maintains the difference
-- between the lengths of @f@ and @r@. @diff@ may be inaccurate during rebuilding, but must be
-- accurate by the time rebuilding is finished.

---------------------------------------------------------------------------------------------------

-- |Prints table of snocs from 1..(n-1)
demoDHMQ :: Int -> IO ()
demoDHMQ = prettyBuildDHMQHistory

---------------------------------------------------------------------------------------------------

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving (Eq, Show, Functor, Foldable)

-- |`Ch8.HoodMelvilleQueue` with single `Int` field for size diff rather than two size fields
data DiffHMQueue a =
  DHM Int [a] (RotationState a) [a]
  deriving (Eq, Show, Functor, Foldable)

-- |`Ch8.HoodMelvilleQueue.rotateStep` using size diff
rotateStep :: RotationState a -> RotationState a
rotateStep = \case
  Reversing ok (x : f) f' (y:r) r' -> Reversing (ok+1) f  (x:f') r (y:r')
  Reversing ok []      f' [y]   r' -> Appending ok     f' (y:r')
  Appending 0  _       r'          -> Done r'
  Appending ok (x:f')  r'          -> Appending (ok-1) f' (x:r')
  state                            -> state

-- |`Ch8.HoodMelvilleQueue.invalidate` using size diff
invalidate :: RotationState a -> RotationState a
invalidate = \case
  Reversing ok f f' r r' -> Reversing (ok-1) f f' r r'
  Appending 0  _  (_:r') -> Done r'
  Appending ok f' r'     -> Appending (ok-1) f' r'
  state                  -> state

-- |`Ch8.HoodMelvilleQueue.rotateTwice` using size diff
rotateTwice :: DiffHMQueue a -> DiffHMQueue a
rotateTwice (DHM diff f state r) =
  case rotateStep $ rotateStep state of
    Done newf -> DHM lenf newf Idle     lenr r
    newstate  -> DHM lenf f    newstate lenr r

-- |`Ch8.HoodMelvilleQueue.rebalance` using size diff
rebalance :: DiffHMQueue a -> DiffHMQueue a
rebalance (DHM diff f state r)
  | lenr <= lenf = rotateTwice $ DHM lenf f state lenr r
  | otherwise    =
    let newstate = Reversing 0 f [] r []
    in  rotateTwice $ DHM (lenf+lenr) f newstate 0 []

instance Queue DiffHMQueue where
  empty :: DiffHMQueue a
  empty = DHM 0 [] Idle 0 []

  isEmpty :: DiffHMQueue a -> Bool
  isEmpty (DHM diff _ _ _) = lenf == 0

  snoc :: DiffHMQueue a -> a -> DiffHMQueue a
  snoc (DHM diff f state r) x =
    rebalance $ DHM lenf f state (lenr+1) (x : r)

  head :: DiffHMQueue a -> Either QueueEmpty a
  head (DHM _ []    _ _) = Left QueueEmpty
  head (DHM _ (x:_) _ _) = Right x

  tail :: DiffHMQueue a -> Either QueueEmpty (DiffHMQueue a)
  tail (DHM _    []    _     _) = Left QueueEmpty
  tail (DHM diff (_:f) state r) = Right $
    rebalance $ DHM (lenf-1) f (invalidate state) lenr r

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Build up a @DiffHMQueue@ from a list
buildDHMQ :: [a] -> DiffHMQueue a
buildDHMQ = foldl' snoc empty

-- |Capture steps for building up a @DiffHMQueue@ from a generated list `1..n`
buildDHMQHistory :: Int -> [DiffHMQueue Int]
buildDHMQHistory n = scanl' snoc empty [1..n]

-- |I am tiger king
prettyBuildDHMQHistory :: Int -> IO ()
prettyBuildDHMQHistory n =
  P.putStrLn . printSides $ buildDHMQHistory n
  where
    prettySides :: [DiffHMQueue Int] -> [(String, String, String, String)]
    prettySides = map
      $ \(DHM diff front rotState rear) ->
        ( show diff
        , show front
        , show rotState
        , show rear
        )
    defCol :: ColSpec
    defCol = column (fixed (n + 1)) center dotAlign def
    sizeCol :: ColSpec
    sizeCol = column (fixed 7) center dotAlign def
    rotCol :: ColSpec
    rotCol = column (fixed (n * 3)) center dotAlign def
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n "snoc ") (map show [1..(n-1)])
    components :: [DiffHMQueue Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [DiffHMQueue Int] -> String
    printSides qs =
      tableString [defCol, sizeCol, defCol, rotCol, defCol]
                  unicodeS
                  (titlesH ["Command", "Diff", "Front", "Rotation", "Rear"])
                  . map rowG . zipWith (:) commands $ components qs
