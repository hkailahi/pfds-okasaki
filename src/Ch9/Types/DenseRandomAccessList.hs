module Ch9.Types.DenseRandomAccessList where

import BasicPrelude hiding (empty)

import Ch9.Classes.RandomAccessList

-- |Implementation of random access lists using dense binary numbers of complete binary leaf trees.
-- This probably calls for size indexed types, actually.

-- |Complete binary leaf trees that cache their size (leaf count) on their nonleaf nodes.

data CBLeafTree a = CBLeaf a | CBNode Integer (CBLeafTree a) (CBLeafTree a)
  deriving (Eq, Show)

-- |Binary numbers constructed from these trees, where a tree in the digit position of weight
-- 2^k will have 2^k leaf nodes.

data DenseRADigit a = DDZero | DDOne (CBLeafTree a)
  deriving (Eq, Show)

-- |And a list of these (which is required by invariant to carry its digits in ascending order
-- of weight) is our data structure.

newtype DenseRAList a = DenseRAList [DenseRADigit a]
  deriving (Eq, Show)

-- |Return the size (leaf count) of a complete binary leaf tree (from the cache).
size :: CBLeafTree a -> Integer
size (CBLeaf _)     = 1
size (CBNode s _ _) = s

-- |Combine two complete binary leaf trees of the same rank into another. If they do not have
-- the same rank then the function still works but the result is not a valid complete binary
-- leaf tree.
link :: CBLeafTree a -> CBLeafTree a -> CBLeafTree a
link t t' = CBNode (size t + size t') t t'

-- |Common helper for head and tail. unconsTree always returns (t, ts) where the rank of t is
-- equal to the weight attached to the low order digit of its argument (which you can't see).
unconsTree :: [DenseRADigit a] -> Either Empty (CBLeafTree a, [DenseRADigit a])
unconsTree = \ case
  []            -> Left Empty
  [DDOne t]     -> Right (t, [])
  DDOne t : ts' -> Right (t, DDZero : ts')
  DDZero : ts'  -> case unconsTree ts' of
    Left Empty                  -> error "can't have empty DenseRAList with any digits in it"
    Right (CBLeaf _, _)         -> error "forbidden by invariant of unconsTree"
    Right (CBNode _ t t', ts'') -> Right (t, DDOne t' : ts'')

-- |Common helper for lookup functions. Yields the jth element of the complete binary leaf tree.
lookupTree :: Integer -> CBLeafTree a -> Either IndexError a
lookupTree 0 (CBLeaf x)      = Right x
lookupTree _ (CBLeaf _)      = Left IndexError
lookupTree j _ | j < 0       = Left IndexError
lookupTree j (CBNode w t t')
  | j < w `div` 2            = lookupTree j t
  | otherwise                = lookupTree (j - w `div` 2) t'

-- |Common helper for update functions. Yields the complete binary leaf tree with the jth element
-- replaced.
updateTree :: Integer -> a -> CBLeafTree a -> Either IndexError (CBLeafTree a)
updateTree 0 y (CBLeaf _)      = Right (CBLeaf y)
updateTree _ _ (CBLeaf _)      = Left IndexError
updateTree j _ _ | j < 0       = Left IndexError
updateTree j y (CBNode w t t')
  | j < w `div` 2              = do
      updatedLeft <- updateTree j y t
      pure $ CBNode w updatedLeft t'
  | otherwise                  = do
      updatedRight <- updateTree (j - w `div` 2) y t'
      pure $ CBNode w t updatedRight

instance RandomAccessList DenseRAList where

  empty = DenseRAList []

  isEmpty (DenseRAList []) = True
  isEmpty _                = False

  cons x (DenseRAList ts) = DenseRAList $ consTree (CBLeaf x) ts
    where
      consTree :: CBLeafTree a -> [DenseRADigit a] -> [DenseRADigit a]
      -- consTree maintains the invariant that it is only ever called with a second argument
      -- whose low order tree is the same rank as its first argument, so it is safe to call
      -- link to combine them.
      consTree t = \ case
        []             -> [DDOne t]
        DDZero : ts'   -> DDOne t : ts'
        DDOne t' : ts' -> DDZero : consTree (link t t') ts'

  head (DenseRAList ts) = case unconsTree ts of
    Left Empty          -> Left Empty
    Right (CBLeaf x, _) -> Right x
    Right _             -> error "forbidden by invariant of unconsTree"

  tail (DenseRAList ts) = DenseRAList . snd <$> unconsTree ts

  lookup i (DenseRAList ts) = lookup' i ts
    where
      lookup' :: Integer -> [DenseRADigit a] -> Either IndexError a
      lookup' j = if j < 0 then \_ -> Left IndexError else \ case
        []             -> Left IndexError
        DDZero : ts'   -> lookup' j ts'
        DDOne t : ts'
          | j < size t -> lookupTree j t
          | otherwise  -> lookup' (j - size t) ts'

  update i x (DenseRAList ts) = DenseRAList <$> update' i x ts
    where
      update' :: Integer -> a -> [DenseRADigit a] -> Either IndexError [DenseRADigit a]
      update' j y = if j < 0 then \_ -> Left IndexError else \ case
        []             -> Left IndexError
        DDZero : ts'   -> (DDZero :) <$> update' j y ts'
        DDOne t : ts'
          | j < size t -> do
              updatedFirst <- updateTree j y t
              pure $ DDOne updatedFirst : ts'
          | otherwise  -> (DDOne t :) <$> update' (j - size t) y ts'

-- |EXERCISE 9.1

-- |drop deletes the first j elements of a random-access list in O(log n) worst case time, by
-- skipping elements until we see which tree we need to take apart. If this tree has size 2^k,
-- it will be disassembled to yield the order 2^0 through 2^k components of the result, and
-- the order 2^(k+1) and higher components of the original list are unchanged. This takes
-- O(log j) time to scan for the tree we need to disassemble and O(k) < O(log n) time to actually
-- take it apart, since we only examine each level of the 2^k tree once; finally we spend
-- O(log n - k) time to append the result of disassembling and the remaining list.

drop :: Integer -> DenseRAList a -> Either IndexError (DenseRAList a)
drop i (DenseRAList ts) = DenseRAList <$> drop' i ts
  where
    drop' :: Integer -> [DenseRADigit a] -> Either IndexError [DenseRADigit a]
    drop' 0 []              = Right []
    drop' _ []              = Left IndexError
    drop' j _ | j < 0       = Left IndexError
    drop' j (DDZero : ts')  = drop' j ts'
    drop' j (DDOne t : ts')
      | j < size t          = Right $ reverse (dropTree j t) ++ ts'
      | otherwise           = drop' (j - size t) ts'

    -- repeatZero appends enough zeros to a single tree to multiply its order by size,
    -- and returns the result in reverse order.
    repeatZero :: Integer -> [DenseRADigit a] -> [DenseRADigit a]
    repeatZero s acc
      | s == 1    = reverse acc
      | otherwise = repeatZero (s `div` 2) (DDZero : acc)

    -- dropTree j t, where 0 <= j < 2^k = size t, returns a list of k + 1 DenseRADigit
    -- representing orders 2^0 through 2^k, and containing the last 2^k - j elements of t.
    -- This list is returned in reverse order.
    dropTree :: Integer -> CBLeafTree a -> [DenseRADigit a]
    dropTree 0 t               = repeatZero (size t) [DDOne t]
    dropTree _ (CBLeaf _)      = error "can't be asked to dropTree on more than tree size"
    dropTree j (CBNode w t t')
      -- If we need to take apart the left half, stick the right half on the front when done
      | j < w `div` 2          = case dropTree j t of
          []           -> error "can't be asked to drop from tree of size 0"
          DDOne _ : _  -> error "can't get leading digit DDOne when dropping j > 0"
          DDZero : ts' -> DDZero : DDOne t' : ts'
      -- If we need to take apart the right half, stick a zero for the left half on the front
      | otherwise              = DDZero : dropTree (j - w `div` 2) t'

-- |EXERCISE 9.2

-- |repeat i x creates a random-access list containing i copies of x. This is done in O(log i)
-- time by ensuring that the tree with 2^k copies of x is created by sticking together two of
-- the (single) tree with 2^(k-1) copies of x.

repeat :: Integer -> a -> DenseRAList a
repeat i x = DenseRAList $ go i (CBLeaf x)
  where
    go :: Integer -> CBLeafTree a -> [DenseRADigit a]
    go j _ | j <= 0 = []
    go j t          =
      let thisDigit = if j `mod` 2 == 1 then DDOne t else DDZero
      in thisDigit : go (j `div` 2) (link t t)
