{-# LANGUAGE AllowAmbiguousTypes #-}

module Ch10.Demo where

import BasicPrelude hiding (empty, head, lookup, tail, uncons)
import Control.Lens (each, toListOf)
import qualified Prelude as P
import Text.Layout.Table
  (ColSpec, center, column, def, dotAlign, fixed, rowG, tableString, titlesH, unicodeS)

import qualified Ch5.Classes.Queue as Q (Queue (..))
import Ch9.Classes.RandomAccessList (RandomAccessList (..))
import Ch10.Types.BootstrapRandomAccessList
import Ch10.Types.BootstrapQueue
import qualified Ch10.Classes.CatenableList as C (CatenableList (..))
import Ch10.Types.CatList

-- |Build up a @PairSeq@ from a list [1..n]
demoPairSeq :: Int -> IO ()
demoPairSeq n = prettyBuildPairSeqHistory n

-- |Build up a @ZerolessPairSeq@ from a list [1..n]
demoZPS :: Int -> IO ()
demoZPS n = prettyBuildZPSHistory n

-- |Build up a @BootstrapQueue@ from a list [1..n]
demoBootQ :: Int -> IO ()
demoBootQ n = prettyBuildBootQHistory n

-- |Build up a @CatenableList@ from a list [1..n]
-- ->>> demoCL @[] 12
-- ...
demoCL :: forall queue. (Foldable queue, Q.Queue queue, Show (queue (CatList queue Int))) => Int -> IO ()
demoCL n = prettyBuildCLHistory @queue n

---------------------------------------------------------------------------------------------------
-- `PairSeq` Utilities

toBinString :: PairSeq a -> String
toBinString = \case
  N      -> ""
  O xs   -> "0" <> toBinString xs
  I _ xs -> "1" <> toBinString xs

buildPairSeq :: [a] -> PairSeq a
buildPairSeq = foldl' (flip cons) empty

-- |Capture steps for building up a @PairSeq@ from a generated list `1..n`
buildPairSeqHistory :: Int -> [PairSeq Int]
buildPairSeqHistory n = scanl' (flip cons) empty [1..n]

-- |I am tiger king
prettyBuildPairSeqHistory :: Int -> IO ()
prettyBuildPairSeqHistory n =
  P.putStrLn . printSides $ buildPairSeqHistory n
  where
    prettySides :: [PairSeq Int] -> [(String, String)]
    prettySides = map (\psq ->
        ( toBinString psq
        , show psq
        )
      )
    defCol :: ColSpec
    defCol = column (fixed 8) center dotAlign def
    sizeCol :: ColSpec
    sizeCol = column (fixed (3 + n `div` 3)) center dotAlign def
    rotCol :: ColSpec
    rotCol = column (fixed (16 + n + (n `div` 2)^(2 :: Int))) center dotAlign def
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n "cons ") (map show [1..(n-1)])
    components :: [PairSeq Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [PairSeq Int] -> String
    printSides qs =
      tableString [defCol, sizeCol, rotCol]
                  unicodeS
                  (titlesH ["Command", "Binary", "Value"])
                  . map rowG . zipWith (:) commands $ components qs

---------------------------------------------------------------------------------------------------
-- `ZerolessPairSeq` Utilities

toNumRepString :: ZerolessPairSeq a -> String
toNumRepString = \case
  ZNil            -> ""
  ZOne _ xs       -> "1" <> toNumRepString xs
  ZTwo _ _ xs     -> "2" <> toNumRepString xs
  ZThree _ _ _ xs -> "3" <> toNumRepString xs

buildZPS :: [a] -> ZerolessPairSeq a
buildZPS = foldl' (flip cons) empty

-- |Capture steps for building up a @ZerolessPairSeq@ from a generated list `1..n`
buildZPSHistory :: Int -> [ZerolessPairSeq Int]
buildZPSHistory n = scanl' (flip cons) empty [1..n]

-- |I am tiger king
prettyBuildZPSHistory :: Int -> IO ()
prettyBuildZPSHistory n =
  P.putStrLn . printSides $ buildZPSHistory n
  where
    prettySides :: [ZerolessPairSeq Int] -> [(String, String)]
    prettySides = map (\psq ->
        ( toNumRepString psq
        , show psq
        )
      )
    defCol :: ColSpec
    defCol = column (fixed 8) center dotAlign def
    sizeCol :: ColSpec
    sizeCol = column (fixed (3 + n `div` 3)) center dotAlign def
    rotCol :: ColSpec
    rotCol = column (fixed (16 + n + (n `div` 2)^(2 :: Int))) center dotAlign def
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n "cons ") (map show [1..(n-1)])
    components :: [ZerolessPairSeq Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [ZerolessPairSeq Int] -> String
    printSides qs =
      tableString [defCol, sizeCol, rotCol]
                  unicodeS
                  (titlesH ["Command", "Num Rep", "Value"])
                  . map rowG . zipWith (:) commands $ components qs

---------------------------------------------------------------------------------------------------
-- `BootstrapQueue` Utilities

-- |Build up a @BootstrapQueue@ from a list
buildBootQ :: [a] -> BootstrapQueue a
buildBootQ = foldl' Q.snoc Q.empty

-- |Capture steps for building up a @BootstrapQueue@ from a generated list `1..n`
buildBootQHistory :: Int -> [BootstrapQueue Int]
buildBootQHistory n = scanl' Q.snoc Q.empty [1..n]

-- |I am tiger king
prettyBuildBootQHistory :: Int -> IO ()
prettyBuildBootQHistory n =
  P.putStrLn . printSides $ buildBootQHistory n
  where
    prettySides :: [BootstrapQueue Int] -> [(String, String, String, String, String)]
    prettySides = map
      (\case
        BQEmpty -> ("0", "", "", "", "0")
        BQ lenFronts front futureFronts lenRear rear ->
          ( show lenFronts
          , show front
          , show (case futureFronts of
                    BQEmpty -> ""
                    BQ lenFronts' front' _ lenRear' rear' ->
                      show lenFronts' <> " "
                      <> show front' <> " "
                      <> show lenRear'  <> " "
                      <> show rear'  <> " "
                 )
          , show lenRear
          , show rear
          )
      )
    defCol :: ColSpec
    defCol = column (fixed (n + 1)) center dotAlign def
    sizeCol :: ColSpec
    sizeCol = column (fixed 7) center dotAlign def
    rotCol :: ColSpec
    rotCol = column (fixed (15 + n * 4)) center dotAlign def
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n "snoc ") (map show [1..(n-1)])
    components :: [BootstrapQueue Int] -> [[String]]
    components qs = map (toListOf each) $ prettySides qs
    printSides :: [BootstrapQueue Int] -> String
    printSides qs =
      tableString [defCol, sizeCol, defCol, rotCol, sizeCol, defCol]
                  unicodeS
                  (titlesH ["Command", "Size(F)", "Front", "Future Front (Size(F),Front,Size(R),Rear)", "Size(R)", "Rear"])
                  . map rowG . zipWith (:) commands $ components qs

---------------------------------------------------------------------------------------------------
-- `CatList` Utilities

-- |Build up a @CatList@ from a list
buildCL :: (Foldable q, Q.Queue q) => [a] -> CatList q a
buildCL = foldl' C.snoc C.empty

-- |Capture steps for building up a @CatList@ from a generated list `1..n`
buildCLHistory :: (Foldable q, Q.Queue q) => Int -> [CatList q Int]
buildCLHistory n = scanl' C.snoc C.empty [1..n]

-- |I am tiger king
prettyBuildCLHistory :: forall queue. (Foldable queue, Q.Queue queue, Show (queue (CatList queue Int))) => Int -> IO ()
prettyBuildCLHistory n =
  P.putStrLn . printSides @(queue) $ buildCLHistory n
  where
    prettySides :: forall q. (Show (q (CatList q Int)))
      => [CatList q Int] -> [String]
    prettySides = map show
    defCol :: ColSpec
    defCol = column (fixed 8) center dotAlign def
    rotCol :: ColSpec
    rotCol = column (fixed (100 + n + (n `div` 3)^(3 :: Int))) center dotAlign def
    commands :: [String]
    commands = ["empty"]
      <> zipWith (<>) (replicate n "cons ") (map show [1..(n-1)])
    components :: forall q. (Show (q (CatList q Int)))
      => [CatList q Int] -> [[String]]
    components qs = map (\x -> [x]) $ prettySides qs
    printSides :: forall q. (Show (q (CatList q Int)))
      => [CatList q Int] -> String
    printSides qs =
      tableString [defCol, rotCol]
                  unicodeS
                  (titlesH ["Command", "Value"])
                  . map rowG . zipWith (:) commands $ components qs
