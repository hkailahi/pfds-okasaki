module Ch8.Types.HoodMelvilleQueue where

import BasicPrelude hiding (empty)
import Control.Lens (each, toListOf)
import qualified Prelude as P
import Text.Layout.Table
  (ColSpec, center, column, def, dotAlign, fixed, rowG, tableString, titlesH, unicodeS)

import Ch5.Classes.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))

-- |Prints table of snocs from 1..(n-1)
demoHMQ :: Int -> IO ()
demoHMQ = prettyBuildHMQHistory

---------------------------------------------------------------------------------------------------

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving (Eq, Show, Functor, Foldable)

-- Similar in many ways to the `RealTimeQueue`.
--   * Maintains two lists representing the front and rear of the queue, respectively
--   * Incrementally rotates elements from the rear list to the front list beginning when the rear
--     list becomes one longer than the front list.
-- The differences lie in the details of this incremental rotation.
data HoodMelvilleQueue a =
  HM Int [a] (RotationState a) Int [a]
  deriving (Eq, Show, Functor, Foldable)

-- |@exec@ in book
rotateStep :: RotationState a -> RotationState a
rotateStep = \case
  Reversing ok (x : f) f' (y:r) r' -> Reversing (ok+1) f  (x:f') r (y:r')
  Reversing ok []      f' [y]   r' -> Appending ok     f' (y:r')
  Appending 0  _       r'          -> Done r'
  Appending ok (x:f')  r'          -> Appending (ok-1) f' (x:r')
  state                            -> state

-- |We keep track of the number of valid elements in @f@, and quit copying elements from
-- @f@ to @r'@ when this number reaches zero. Every call to `tail` during the rotation
-- decrements the number of valid elements.
invalidate :: RotationState a -> RotationState a
invalidate = \case
  Reversing ok f f' r r' -> Reversing (ok-1) f f' r r'
  Appending 0  _  (_:r') -> Done r'
  Appending ok f' r'     -> Appending (ok-1) f' r'
  state                  -> state

-- |@exec2@ in book
--
-- Q: How many calls to exec we must issue per `snoc` and `tail` to guarantee that the rotation
-- completes before either the next rotation is ready to begin or the working copy of the front
-- list is exhausted?
--
-- A: Twice! Assume that @f@ has length @m@ and @r@ has length @m + 1@ at the beginning of a
-- rotation. Then, the next rotation will begin after any combination of @2m+2@ insertions or
-- deletions, but the working copy of the front list will be exhausted after just @m@ deletions.
-- Altogether, the rotation requires at most @2m + 2@ steps to complete. If we call exec twice
-- per operation, including the operation that begins the rotation, then the rotation will complete
-- at most @m@ operations after it begins.
rotateTwice :: HoodMelvilleQueue a -> HoodMelvilleQueue a
rotateTwice (HM lenf f state lenr r) =
  case rotateStep $ rotateStep state of
    Done newf -> HM lenf newf Idle     lenr r
    newstate  -> HM lenf f    newstate lenr r

-- |@check@ in book
rebalance :: HoodMelvilleQueue a -> HoodMelvilleQueue a
rebalance (HM lenf f state lenr r)
  | lenr <= lenf = rotateTwice $ HM lenf f state lenr r
  | otherwise    =
    let newstate = Reversing 0 f [] r []
    in  rotateTwice $ HM (lenf+lenr) f newstate 0 []

instance Queue HoodMelvilleQueue where
  empty :: HoodMelvilleQueue a
  empty = HM 0 [] Idle 0 []

  isEmpty :: HoodMelvilleQueue a -> Bool
  isEmpty (HM lenf _ _ _ _) = lenf == 0

  snoc :: HoodMelvilleQueue a -> a -> HoodMelvilleQueue a
  snoc (HM lenf f state lenr r) x =
    rebalance $ HM lenf f state (lenr+1) (x : r)

  head :: HoodMelvilleQueue a -> Either QueueEmpty a
  head (HM _ []    _ _ _) = Left QueueEmpty
  head (HM _ (x:_) _ _ _) = Right x

  tail :: HoodMelvilleQueue a -> Either QueueEmpty (HoodMelvilleQueue a)
  tail (HM _    []    _     _    _) = Left QueueEmpty
  tail (HM lenf (_:f) state lenr r) = Right $
    rebalance $ HM (lenf-1) f (invalidate state) lenr r

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Build up a @HoodMelvilleQueue@ from a list
buildHMQ :: [a] -> HoodMelvilleQueue a
buildHMQ = foldl' snoc empty

-- |Capture steps for building up a @HoodMelvilleQueue@ from a generated list `1..n`
buildHMQHistory :: Int -> [HoodMelvilleQueue Int]
buildHMQHistory n = scanl' snoc empty [1..n]

-- |I am tiger king
prettyBuildHMQHistory :: Int -> IO ()
prettyBuildHMQHistory n =
  P.putStrLn . printSides $ buildHMQHistory n
  where
    prettySides :: [HoodMelvilleQueue Int] -> [(String, String, String, String, String)]
    prettySides = map
      $ \(HM lF front rotState lR rear) ->
        ( show lF
        , show front
        , show rotState
        , show lR
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
    components :: [HoodMelvilleQueue Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [HoodMelvilleQueue Int] -> String
    printSides qs =
      tableString [defCol, sizeCol, defCol, rotCol, sizeCol, defCol]
                  unicodeS
                  (titlesH ["Command", "Size(F)", "Front", "Rotation", "Size(R)", "Rear"])
                  . map rowG . zipWith (:) commands $ components qs
