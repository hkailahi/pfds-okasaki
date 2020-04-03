module Ch7.Types.RealTimeQueue where

import BasicPrelude hiding (empty)
import Control.Lens (each, toListOf)
import GHC.Exts (toList)
import qualified Prelude as P
import qualified StrictList as SL (List (Cons, Nil))
import Text.Layout.Table
  (ColSpec, center, column, def, dotAlign, fixed, rowG, tableString, titlesH, unicodeS)

import Ch5.Classes.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))

-- |Variant of banker's queue that for worst-case instead of amortized time operations.
-- Uses incremental rotations, whereas the orginal used the monolithic `reverse`.
data RealTimeQueue a =
  RealTimeQueue
    [a]          -- ^ front
    (SL.List a)  -- ^ rear
    [a]          -- ^ Schedule/Accumulating parameter: accumulates the partial results of reversing
                --  rear. It is initially empty.
  deriving (Eq, Show, Functor, Foldable)

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Building up a `ProgressQueue` from a SL.List
buildPQ :: [a] -> RealTimeQueue a
buildPQ = foldl' snoc empty

---------------------------------------------------------------------------------------------------

-- |Constructs a queue from parts, performing a computation based on schedule.
--   * If the schedule is empty, frontload and refill schedule
--   * Otherwise, advance one step through schedule
mkValid :: [a] -> SL.List a -> [a] -> RealTimeQueue a
mkValid front rear [] = RealTimeQueue zs SL.Nil zs
  where
    zs = rotate front rear []
mkValid front rear (_ : as) = RealTimeQueue front rear as


-- |Incremental reversal of queue components.
--   * If the schedule is empty, frontload and refill schedule
--   * Otherwise, advance one step through schedule
rotate :: [a] -> SL.List a -> [a] -> [a]
rotate []       (SL.Cons r SL.Nil) acc = r : acc
rotate (f : fs) (SL.Cons r rs)     acc = f : rotate fs rs (r : acc)
rotate _ _ _                           = error "Should be SL.NonEmpty but aintnobodygottimefodat"

---------------------------------------------------------------------------------------------------

instance Queue RealTimeQueue where
  empty :: RealTimeQueue a
  empty = RealTimeQueue [] SL.Nil []

  isEmpty :: RealTimeQueue a -> Bool
  isEmpty (RealTimeQueue front _ _) = null front

  head :: RealTimeQueue a -> Either QueueEmpty a
  head (RealTimeQueue (f:_) _ _) = Right f
  head _                         = Left QueueEmpty

  tail :: RealTimeQueue a -> Either QueueEmpty (RealTimeQueue a)
  tail (RealTimeQueue (_:fs) rear acc) = Right $ mkValid fs rear acc
  tail _                               = Left QueueEmpty

  snoc :: RealTimeQueue a -> a -> RealTimeQueue a
  snoc (RealTimeQueue front rear acc) x = mkValid front (SL.Cons x rear) acc

---------------------------------------------------------------------------------------------------

-- |Building up a `RealTimeQueue` from a list
buildRTQ :: [a] -> RealTimeQueue a
buildRTQ = foldl' snoc empty

-- |Steps of building up a RealTimeQueue from a generated list 1..n
buildRTQHistory :: Int -> [RealTimeQueue Int]
buildRTQHistory n = scanl' snoc empty [1..n]

-- |I am tiger king
prettyBuildRTQHistory :: Int -> IO ()
prettyBuildRTQHistory n =
  P.putStrLn . printSides $ buildRTQHistory n
  where
    prettySides :: [RealTimeQueue Int] -> [(String, String, String)]
    prettySides = map
      $ \(RealTimeQueue front rear acc) -> (show front, show (toList rear), show acc)
    defCol :: ColSpec
    defCol = column (fixed (n*2)) center dotAlign def
    commands :: [String]
    commands = zipWith (<>) (replicate n "snoc ") (map show [1..n])
    components :: [RealTimeQueue Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [RealTimeQueue Int] -> String
    printSides qs =
      tableString [defCol, defCol, defCol, defCol]
                  unicodeS
                  (titlesH ["Command", "Front", "Rear", "Schedule"])
                  . map rowG . zipWith (:) commands $ components qs

-- TODO
-- prettyStateRTQHistory :: [RealTimeQueue a -> RealTimeQueue a] -> IO ()
-- prettyStateRTQHistory commands =
--   P.putStrLn . printSides $ buildRTQHistory n
--   where
--     n = len commands
--     prettySides :: [RealTimeQueue Int] -> [(String, String, String)]
--     prettySides = map
--       $ \(RealTimeQueue front rear acc) -> (show front, show (toList rear), show acc)
--     defCol = column (fixed (n*2)) center dotAlign def
--     printSides :: [RealTimeQueue Int] -> String
--     printSides qs =
--       tableString [defCol, defCol, defCol]
--                   unicodeS
--                   (titlesH ["Front", "Rear", "Schedule"])
--                   $ map (rowG . toListOf each) $ prettySides qs

{-
You can see the pattern:
  * Every time we frontload, we fill the schedule with the entire queue
  * Every time we snoc, we remove one from the schedule
  * Empty schedule is "complete"
    * On empty schedule, both front and rear are the same size
    * We are ready to frontload again on next operation

λ> prettyBuildRTQHistory 10
┌──────────────────────┬──────────────────────┬──────────────────────┬──────────────────────┐
│       Command        │        Front         │         Rear         │       Schedule       │
╞══════════════════════╪══════════════════════╪══════════════════════╪══════════════════════╡
│        snoc 1        │          []          │          []          │          []          │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 2        │         [1]          │          []          │         [1]          │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 3        │         [1]          │         [2]          │          []          │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 4        │       [1,2,3]        │          []          │       [1,2,3]        │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 5        │       [1,2,3]        │         [4]          │        [2,3]         │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 6        │       [1,2,3]        │        [5,4]         │         [3]          │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 7        │       [1,2,3]        │       [6,5,4]        │          []          │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 8        │   [1,2,3,4,5,6,7]    │          []          │   [1,2,3,4,5,6,7]    │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│        snoc 9        │   [1,2,3,4,5,6,7]    │         [8]          │    [2,3,4,5,6,7]     │
├──────────────────────┼──────────────────────┼──────────────────────┼──────────────────────┤
│       snoc 10        │   [1,2,3,4,5,6,7]    │        [9,8]         │     [3,4,5,6,7]      │
└──────────────────────┴──────────────────────┴──────────────────────┴──────────────────────┘
-}
