module Ch9.Classes.BinaryNumber where

import BasicPrelude

-- |Type class signature for binary numbers extracted from Fig 9.1.
-- a is the type Nat of the signature.

class BinaryNumber a where

  zero :: a
  -- ^ Local representation of zero.
  
  inc :: a -> a
  -- ^ Increment a number by one.

  dec :: a -> Maybe a
  -- ^ Decrement a strictly positive number by one; decrementing zero gives Nothing.

  add :: a -> a -> a
  -- ^ Add two numbers.
