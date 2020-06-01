module Ch9.Types.ZerolessDenseBinary where

import BasicPrelude

import Ch9.Classes.BinaryNumber

-- |Implementation of binary numbers as zeroless, i.e., the permitted digits are 1 and 2.

-- |Digit type. X for two since it has two lines.

data ZLBit = I | X
  deriving (Eq, Show)

type ZerolessDenseBinaryNat = [ZLBit]

instance BinaryNumber ZerolessDenseBinaryNat where

  zero = []

  inc []       = [I]
  inc (I : ds) = X : ds
  inc (X : ds) = I : inc ds

  -- EXERCISE 9.4. Add implementations of dec and add for this type.

  dec []       = Nothing
  dec [I]      = Just []
  dec (X : ds) = Just (I : ds)
  dec (I : ds) = case dec ds of
    Nothing  -> error "excluded by previous case"
    Just ds' -> Just (X : ds')

  add ds       []        = ds
  add []       ds        = ds
  add (I : ds) (I : ds') = X : add ds ds'
  add (I : ds) (X : ds') = I : inc (add ds ds')
  add (X : ds) (I : ds') = I : inc (add ds ds')
  add (X : ds) (X : ds') = X : inc (add ds ds')
