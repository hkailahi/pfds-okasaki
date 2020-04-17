{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, QuantifiedConstraints #-}

module Demoable where

import BasicPrelude hiding (empty, insert)
import Data.Functor.Classes (Show1)
import Control.Lens (each, toListOf)
import qualified Prelude as P
import Text.Layout.Table
  (ColSpec, center, column, def, dotAlign, fixed, rowG, tableString, titlesH, unicodeS)

import qualified Ch5.Classes.Queue as Queue (Queue (empty, snoc))

-- FIXME This be WIP, should be able to get free instances for Queue, Set, Dequeue, etc
-- TODO
--  * Defunctionalize the commands
--    * Enables writing demo with nice DSL and attaching custom interpreters (execute actual function on input, showCommand, etc)
--  * Smarter sizing
--  * Figure out how to do newlines
--     * Follow-up: Allow visual reps (ex. pretty printing trees)

data DemoCol = DemoCol
  { _demoColName :: String
  , _demoColSize :: Int
  }
  deriving (Eq, Show)

names :: [DemoCol] -> [String]
names = map _demoColName

sizes :: [DemoCol] -> [Int]
sizes = map _demoColSize

class (Show1 f, Foldable f) => Demoable f where
  empty   :: f a
  insert  :: f a -> a -> f a
  showCommand :: String
  -- TODO Separate into class, rest are default from Queue/Stack/Etc
  -- But these are specific to structure
  showRow :: (Show a) => f a -> [String]
  columns :: [DemoCol] -- Names and sizes

-- instance (Stack f) => (Demoable f)
instance (Show1 f, Foldable f, Queue.Queue f) => Demoable f where
  empty = Queue.empty
  insert = Queue.snoc
  showCommand = "snoc"
  showRow _ = [""]
  columns = [DemoCol "Header1" 10]


-- |Prints table of snocs from 1..(n-1)
demoInserts :: forall f. (Demoable f) => Int -> IO ()
demoInserts = prettyBuildDemoHistory @f

-- |Build up a @HoodMelvilleQueue@ from a list
buildDemo :: (Demoable f) => [a] -> f a
buildDemo = foldl' insert empty

---------------------------------------------------------------------------------------------------
-- Utilities

-- |Capture steps for building up a @HoodMelvilleQueue@ from a generated list `1..n`
buildDemoHistory :: (Demoable f) => Int -> [f Int]
buildDemoHistory n = scanl' insert empty [1..n]

-- |I am tiger king
prettyBuildDemoHistory :: forall f. (Demoable f) => Int -> IO ()
prettyBuildDemoHistory n =
  P.putStrLn . printSides $ buildDemoHistory n
  where
    cols :: [DemoCol]
    cols = columns @f
    command :: String
    command = showCommand @f
    prettySides :: [f Int] -> [[String]]
    prettySides = map showRow
    mkColHeaders :: [Int] -> [ColSpec]
    mkColHeaders = map (\x -> column (fixed (x * 0 + n + 1)) center dotAlign def)
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n $ command <> " ") (map show [1..(n-1)])
    components :: [f Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [f Int] -> String
    printSides qs =
      tableString (mkColHeaders $ sizes cols)
                  unicodeS
                  (titlesH $ names cols)
                  . map rowG . zipWith (:) commands $ components qs
