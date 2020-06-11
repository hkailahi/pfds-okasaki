module Ch9.Demo where

import BasicPrelude hiding (empty)
import Data.Maybe (fromJust)

import Ch9.Classes.BinaryNumber
import Ch9.Classes.RandomAccessList

---------------------------------------------------------------------------------------------------
-- Binary Operations

-- λ> demoInc @SparseByWeightBinaryNat 4
-- [4]
-- λ> demoInc @SparseByWeightBinaryNat 10
-- [2,8]
-- λ> demoInc @SparseByWeightBinaryNat 64
-- [64]
-- λ> demoInc @SparseByWeightBinaryNat 63
-- [1,2,4,8,16,32]
-- λ> demoInc @DenseBinaryNat 63
-- [I,I,I,I,I,I]
-- λ> demoInc @DenseBinaryNat 64
-- [O,O,O,O,O,O,I]
-- λ> demoInc @DenseBinaryNat 123
-- [I,I,O,I,I,I,I]
-- λ> demoInc @ZerolessDenseBinaryNat 15
-- [I,I,I,I]
-- λ> demoInc @ZerolessDenseBinaryNat 19
-- [I,I,X,I]
-- λ> demoInc @ZerolessDenseBinaryNat 1123
-- [I,I,X,I,I,X,X,I,I,I]
demoInc :: forall a. (BinaryNumber a) => Int -> a
demoInc n = flip (!!) n $ iterate inc zero

-- λ> let x = demoInc 64 :: SparseByWeightBinaryNat
-- λ> let y = demoInc 64 :: DenseBinaryNat
-- λ> demoDec x 20
-- [4,8,32]
-- λ> demoDec x 30
-- [2,32]
-- λ> demoDec x 1
-- [1,2,4,8,16,32]
-- λ> demoDec y 20
-- [O,O,I,I,O,I]
-- λ> demoDec y 30
-- [O,I,O,O,O,I]
-- λ> demoDec y 1
-- [I,I,I,I,I,I]
demoDec :: forall a. (BinaryNumber a) => a -> Int -> a
demoDec bin n = flip (!!) n $ iterate (fromJust . dec) bin

-- λ> Ch9.Classes.BinaryNumber.add x x
-- [128]
-- Demo Add

---------------------------------------------------------------------------------------------------
-- RandomAccessList

-- λ> demoCons @DenseRAList 1
-- DenseRAList [DDOne (CBLeaf '!')]
-- λ> demoCons @SparseRAList 1
-- SparseRAList [CBLeaf '!']
-- λ> demoCons @ZerolessRAList 1
-- ZerolessRAList [I (CBLeaf '!')]

-- λ> demoCons @DenseRAList 4
-- DenseRAList [DDZero,DDZero,DDOne (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]
-- λ> demoCons @SparseRAList 4
-- SparseRAList [CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..)]
-- λ> demoCons @ZerolessRAList 4
-- ZerolessRAList [X CBLeaf.. CBLeaf..,I (CBNode 2 CBLeaf.. CBLeaf..)]

-- λ> demoCons @DenseRAList 7
-- DenseRAList [DDOne CBLeaf..,DDOne (CBNode 2 CBLeaf.. CBLeaf..),DDOne (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]
-- λ> demoCons @SparseRAList 7
-- SparseRAList [CBLeaf '!',CBNode 2 CBLeaf.. CBLeaf..,CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..)]
-- λ> demoCons @ZerolessRAList 7
-- ZerolessRAList [I CBLeaf..,I (CBNode 2 CBLeaf.. CBLeaf..),I (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]
demoCons :: (RandomAccessList f) => Int -> f Char
demoCons n = flip (!!) n $ iterate (cons '!') empty


{-

λ> demoCons @DenseRAList 1
DenseRAList [DDOne (CBLeaf '!')]
λ> demoCons @SparseRAList 1
SparseRAList [CBLeaf '!']
λ> demoCons @ZerolessRAList 1
ZerolessRAList [I (CBLeaf '!')]

λ> demoCons @DenseRAList 4
DenseRAList [DDZero,DDZero,DDOne (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]
λ> demoCons @SparseRAList 4
SparseRAList [CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..)]
λ> demoCons @ZerolessRAList 4
ZerolessRAList [X CBLeaf.. CBLeaf..,I (CBNode 2 CBLeaf.. CBLeaf..)]

λ> demoCons @DenseRAList 7
DenseRAList [DDOne CBLeaf..,DDOne (CBNode 2 CBLeaf.. CBLeaf..),DDOne (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]
λ> demoCons @SparseRAList 7
SparseRAList [CBLeaf '!',CBNode 2 CBLeaf.. CBLeaf..,CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..)]
λ> demoCons @ZerolessRAList 7
ZerolessRAList [I CBLeaf..,I (CBNode 2 CBLeaf.. CBLeaf..),I (CBNode 4 (CBNode 2 CBLeaf.. CBLeaf..) (CBNode 2 CBLeaf.. CBLeaf..))]

-------------------------------------------------------------------
|| # | DenseRAList        | SparseRAList  | ZerolessRAList       ||
-------------------------------------------------------------------
| 1  | [1 /]              | [/]           |  [I]                  |
-------------------------------------------------------------------
| 4  | [0, 0, 1 4  ]      | [  4  ]       |  [X /\, I 2 ]         |
|    |         / \        |   / \         |          / \          |
|    |        2   2       |  2   2        |                       |
|    |       / \ / \      | / \ / \       |                       |
-------------------------------------------------------------------
| 7  | [1 /, 1 2 , 1 4  ] | [/, 2 ,  4  ] |  [I /, I 2 , I  4 ]   |
|    |        / \   / \   |    / \  / \   |         / \    / \    |
|    |             2   2  |        2   2  |               2   2   |
|    |            / \ / \ |       / \ / \ |              / \ / \  |
-------------------------------------------------------------------

-}
