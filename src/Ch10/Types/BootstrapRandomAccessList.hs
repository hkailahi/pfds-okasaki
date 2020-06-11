module Ch10.Types.BootstrapRandomAccessList where

import BasicPrelude hiding (empty, head, lookup, tail, uncons)

import Ch9.Classes.RandomAccessList (Empty (..), IndexError (..), RandomAccessList (..))

-- |Data type for binary lists whose tree size increases by one at each iteration.
data PairSeq a = N | O (PairSeq (a, a)) | I a (PairSeq (a, a))
  deriving (Eq, Show)

-- |Helper for head and tail, as usual.
uncons :: PairSeq a -> Either Empty (a, PairSeq a)
uncons = \ case
  N      -> Left Empty
  I x ps -> Right (x, O ps)
  O ps   -> do
    ((x, y), ps') <- uncons ps
    pure (x, I y ps')

instance RandomAccessList PairSeq where

  empty = N

  isEmpty N = True
  isEmpty _ = False

  cons x = \ case
    N      -> I x N
    O ps   -> I x ps
    I y ps -> O $ cons (x, y) ps

  head xs = fst <$> uncons xs

  tail xs = snd <$> uncons xs

  lookup _ N                    = Left IndexError
  lookup 0 (I x _)              = Right x
  lookup j (I _ ps) | j < 0     = Left IndexError
                    | otherwise = lookup (j - 1) $ O ps
  lookup j (O ps)   | j < 0     = Left IndexError
                    | otherwise = do
                        (x, y) <- lookup (j `div` 2) ps
                        pure $ if j `mod` 2 == 0 then x else y

  update _ _ N                    = Left IndexError
  update 0 e (I _ ps)             = Right $ I e ps
  update j e (I x ps) | j < 0     = Left IndexError
                      | otherwise = cons x <$> update (j - 1) e (O ps)
  update j e (O ps)   | j < 0     = Left IndexError
                      | otherwise = do
                          (x, y) <- lookup (j `div` 2) ps
                          let p' = if j `mod` 2 == 0 then (e, y) else (x, e)
                          O <$> update (j `div` 2) p' ps

{-| EXERCISE 10.1. Prove that this version of `update` runs in O(log^2 n) time.

Let T(n) be the running time of `update` on a sequence of length n. We need to know that
`lookup` takes time O(log n), which is clear from the implementation, since its execution trace
is a series of halving recursions separated by at most one -1 recursion.

If n is odd, `update` goes down the I branch, which takes time 1 in addition to
T(n - 1) (this is the constant-time branch of `cons`).

If n is even, then we spend O(log n/2) executing `lookup`, and an additional T(n/2) recursing
for the `update`. Thus, as with `lookup`, we have at most one -1 recursion between each halving
recursion, so we can neglect the constant time steps and say

T(n) = O(log n/2) + T(n/2) = O(log n) + T(n/2)

Since we will execute at most log n halving recursions, we find T(n) = O(log^2 n).
-}

-- |Improved version of update that takes only O(log n) time by passing the pair updater
-- down the recursion.
update' :: Integer -> a -> PairSeq a -> Either IndexError (PairSeq a)
update' i e = fupdate i (const e)
  where
    fupdate :: Integer -> (a -> a) -> PairSeq a -> Either IndexError (PairSeq a)
    fupdate _ _ N                    = Left IndexError
    fupdate 0 f (I x ps)             = Right $ I (f x) ps
    fupdate j f (I x ps) | j < 0     = Left IndexError
                         | otherwise = cons x <$> fupdate (j - 1) f (O ps)
    fupdate j f (O ps)   | j < 0     = Left IndexError
                         | otherwise =
                             let f' (x, y) = if j `mod` 2 == 0 then (f x, y) else (x, f y)
                             in O <$> fupdate (j `div` 2) f' ps

{-| EXERCISE 10.2. Reimplement this list type with a zeroless representation to obtain O(1)
amortized time for all of `cons`, `head`, `tail`. -}

-- |List structure in zeroless redundant binary numbers. We impose a safe/dangerous constraint
-- as in the case of section 9.2.3: a cons/tail that processes a ZTwo is guaranteed to take
-- constant time, whereas if it processes a ZOne (for tail) or ZThree (for cons) it might
-- continue; but it always transforms that ZOne or ZThree into a ZTwo, so the next operation
-- at that location will not recurse past it. This gets us the O(1) amortized bound as in
-- Exercise 9.8/9.9.

data ZerolessPairSeq a =
    ZNil
  | ZOne a (ZerolessPairSeq (a, a))
  | ZTwo a a (ZerolessPairSeq (a, a))
  | ZThree a a a (ZerolessPairSeq (a, a))
  deriving (Eq, Show)

unconsZ :: ZerolessPairSeq a -> Either Empty (a, ZerolessPairSeq a)
unconsZ = \ case
  ZNil            -> Left Empty
  ZOne x ZNil     -> Right (x, ZNil)
  ZOne x ps       -> do
    ((x', y'), ps') <- unconsZ ps
    pure (x, ZTwo x' y' ps')
  ZTwo x y ps     -> Right (x, ZOne y ps)
  ZThree x y z ps -> Right (x, ZTwo y z ps)

instance RandomAccessList ZerolessPairSeq where

  empty = ZNil

  isEmpty ZNil = True
  isEmpty _    = False

  cons e = \ case
    ZNil            -> ZOne e ZNil
    ZOne x ps       -> ZTwo e x ps
    ZTwo x y ps     -> ZThree e x y ps
    ZThree x y z ps -> ZTwo e x $ cons (y, z) ps

  head = \ case
    ZNil           -> Left Empty
    ZOne x _       -> Right x
    ZTwo x _ _     -> Right x
    ZThree x _ _ _ -> Right x

  tail = map snd . unconsZ

  lookup _ ZNil = Left IndexError
  lookup 0 (ZOne x _)  = Right x
  lookup j (ZOne _ ps)
    | j < 0 = Left IndexError
    | otherwise = do
        (x', y') <- lookup ((j - 1) `div` 2) ps
        pure $ if (j - 1) `mod` 2 == 0 then x' else y'
  lookup 0 (ZTwo x _ _)  = Right x
  lookup j (ZTwo _ y ps)
    | j < 0              = Left IndexError
    | otherwise          = lookup (j - 1) $ ZOne y ps
  lookup 0 (ZThree x _ _ _)  = Right x
  lookup 1 (ZThree _ y _ _)  = Right y
  lookup j (ZThree _ _ z ps)
    | j < 0                  = Left IndexError
    | otherwise              = lookup (j - 2) $ ZOne z ps

  update i e = fupdate i (const e)
    where
      fupdate :: Integer -> (a -> a) -> ZerolessPairSeq a -> Either IndexError (ZerolessPairSeq a)
      fupdate _ _ ZNil = Left IndexError
      fupdate 0 f (ZOne x ps) = Right $ ZOne (f x) ps
      fupdate j f (ZOne x ps)
        | j < 0     = Left IndexError
        | otherwise = ZOne x <$> fupdate ((j - 1) `div` 2) (promote (j - 1) f) ps
      fupdate 0 f (ZTwo x y ps) = Right $ ZTwo (f x) y ps
      fupdate 1 f (ZTwo x y ps) = Right $ ZTwo x (f y) ps
      fupdate j f (ZTwo x y ps)
        | j < 0     = Left IndexError
        | otherwise = ZTwo x y <$> fupdate ((j - 2) `div` 2) (promote (j - 2) f) ps
      fupdate 0 f (ZThree x y z ps) = Right $ ZThree (f x) y z ps
      fupdate 1 f (ZThree x y z ps) = Right $ ZThree x (f y) z ps
      fupdate 2 f (ZThree x y z ps) = Right $ ZThree x y (f z) ps
      fupdate j f (ZThree x y z ps)
        | j < 0     = Left IndexError
        | otherwise = ZThree x y z <$> fupdate ((j - 3) `div` 2) (promote (j - 3) f) ps

      promote :: Integer -> (a -> a) -> (a, a) -> (a, a)
      promote j f (x, y) = if j `mod` 2 == 0 then (f x, y) else (x, f y)
