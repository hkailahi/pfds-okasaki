module Ch9.Types.ZerolessRandomAccessList where

import BasicPrelude hiding (empty)

import Ch9.Classes.RandomAccessList
import Ch9.Types.DenseRandomAccessList
  ( CBLeafTree (CBLeaf, CBNode)
  , link
  , lookupTree
  , size
  , updateTree
  )

-- |EXERCISE 9.5

-- |Implementation of random access lists using dense zeroless binary numbers of complete
-- binary leaf trees. The trees in slot k of the list are required to have size 2^k, as usual.

data ZerolessRADigit a = I (CBLeafTree a) | X (CBLeafTree a) (CBLeafTree a)
  deriving (Eq, Show)

newtype ZerolessRAList a = ZerolessRAList [ZerolessRADigit a]
  deriving (Eq, Show)

instance RandomAccessList ZerolessRAList where

  empty = ZerolessRAList []

  isEmpty (ZerolessRAList []) = True
  isEmpty _                   = False

  cons x (ZerolessRAList tts) = ZerolessRAList $ consTree (CBLeaf x) tts
    where
      consTree :: CBLeafTree a -> [ZerolessRADigit a] -> [ZerolessRADigit a]
      -- As in the case of DenseRAList, consTree t ts maintains the invariant that it is only ever
      -- called with rank t = rank (hd ts).
      consTree s = \ case
        []          -> [I s]
        I t    : ts -> X s t : ts
        X t t' : ts -> I s : consTree (link t t') ts

  head (ZerolessRAList tts) = case tts of
    []                          -> Left Empty
    I (CBLeaf x)            : _ -> Right x
    X (CBLeaf x) (CBLeaf _) : _ -> Right x
    _                           -> error "forbidden by invariant of ZerolessRAList"

  tail (ZerolessRAList tts) = ZerolessRAList . snd <$> unconsTree tts
    where
      unconsTree :: [ZerolessRADigit a] -> Either Empty (CBLeafTree a, [ZerolessRADigit a])
      unconsTree = \ case
        []          -> Left Empty
        [I t]       -> Right (t, [])
        X t t' : ts -> Right (t, I t' : ts)
        I t : ts    -> case unconsTree ts of
          Left Empty                 -> error "can't have empty ZerolessRAList with digits in it"
          Right (CBLeaf _, _)        -> error "forbidden by invariant of ZerolessRAList"
          Right (CBNode _ u u', us) -> Right (t, X u u' : us)

  lookup i (ZerolessRAList tts) = lookup' i tts
    where
      lookup' :: Integer -> [ZerolessRADigit a] -> Either IndexError a
      lookup' j = if j < 0 then \_ -> Left IndexError else \ case
        []                 -> Left IndexError
        I t : ts
          | j < size t     -> lookupTree j t
          | otherwise      -> lookup' (j - size t) ts
        X t t' : ts
          | j < size t     -> lookupTree j t
          | j < 2 * size t -> lookupTree (j - size t) t'
          | otherwise      -> lookup' (j - 2 * size t) ts
      

  update i x (ZerolessRAList tts) = ZerolessRAList <$> update' i x tts
    where
      update' :: Integer -> a -> [ZerolessRADigit a] -> Either IndexError [ZerolessRADigit a]
      update' j y = if j < 0 then \_ -> Left IndexError else \ case
        []                 -> Left IndexError
        I t : ts
          | j < size t     -> do
              updatedFirst <- updateTree j y t
              pure $ I updatedFirst : ts
          | otherwise      -> (I t :) <$> update' (j - size t) y ts
        X t t' : ts
          | j < size t     -> do
              updatedFirst <- updateTree j y t
              pure $ X updatedFirst t' : ts
          | j < 2 * size t -> do
              updatedSecond <- updateTree (j - size t) y t'
              pure $ X t updatedSecond : ts
          | otherwise      -> (X t t' :) <$> update' (j - 2 * size t) y ts

-- |EXERCISE 9.6

-- |lookup and update each take O(log i) time because they are now guaranteed to examine at
-- most log i nodes of the list (since each node is nonempty, i.e., contains at least one tree),
-- and the tree that gets updated has size at most 2i, so updating it takes time O(log 2i)
-- O(log i) as well.
