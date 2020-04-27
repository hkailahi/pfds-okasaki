{-# OPTIONS_GHC -Wno-orphans #-}

module Ch9.Types.SparseByWeightBinary where

import BasicPrelude

import Ch9.Classes.BinaryNumber

-- |Implementation of binary numbers as sparse lists of digit values. Valid values of this type
-- are lists of integers, each member of which is a power of two, and the members of which appear
-- in strictly increasing order. The value of the number represented by a list is the sum of all
-- members of the list.
--
-- That is, the unique representation of 0 is []; of 7 is [1, 2, 4]; of 33 is [1, 32].

type SparseByWeightBinaryNat = [Integer]

-- |Addition helper: yield the sparse binary representation of the number given by adding the
-- (single power of two digit) first argument to the number given by the second argument,
-- under the assumption that the first argument is <= the low order positive digit of the second.
carry :: Integer -> SparseByWeightBinaryNat -> SparseByWeightBinaryNat
carry w []          = [w]
carry w ws@(w':ws')
  | w < w'          = w:ws
  -- NB. this case is really w == w'; the case w > w' yields nonsense, but we don't use it
  | otherwise       = carry (2 * w) ws'

-- |Subtraction helper: yield the sparse binary representation of the number given by subtracting
-- the (single power of two digit) first argument from the number given by the second argument,
-- under the assumption that the first argument is <= the low order positive digit of the second.
-- Yields Nothing if we are trying to borrow from 0.
borrow :: Integer -> SparseByWeightBinaryNat -> Maybe SparseByWeightBinaryNat
borrow _ []          = Nothing
borrow w ws@(w':ws')
  | w == w'          = Just ws'
  -- NB. this case is really w < w'; the case w > w' yields nonsense, but we don't use it
  | otherwise        = case borrow (2 * w) ws of
      Nothing   -> error "can't happen, but type checker won't prove it for us"
      Just ws'' -> Just (w:ws'')

instance BinaryNumber SparseByWeightBinaryNat where
  zero = []

  inc = carry 1

  dec = borrow 1

  add ws       []         = ws
  add []       ws         = ws
  add m@(w:ws) n@(w':ws')
    | w < w'    = w:add ws n
    | w' < w    = w':add m ws'
    | otherwise = carry (2 * w) $ add ws ws'
