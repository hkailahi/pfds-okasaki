module Ch9.Types.DenseBinary where

import BasicPrelude

import Ch9.Classes.BinaryNumber

-- |Binary digit datatype (these names are stolen from Coq)

data Bit = O | I
  deriving (Eq, Show)

-- |Implementation of binary numbers as dense lists of binary digits, least significant bit first.

type DenseBinaryNat = [Bit]

instance BinaryNumber DenseBinaryNat where
  zero = []

  inc []     = [I]
  inc (O:ds) = I:ds
  inc (I:ds) = O:inc ds

  dec []     = Nothing
  dec [I]    = Just []
  dec (I:ds) = Just (O:ds)
  dec (O:ds) = case dec ds of
    Nothing  -> error "[O] is not a valid representation"
    Just ds' -> Just (I:ds')

  add ds     []      = ds
  add []     ds      = ds
  add (d:ds) (O:ds') = d:add ds ds'
  add (O:ds) (d:ds') = d:add ds ds'
  add (I:ds) (I:ds') = O:inc (add ds ds')
