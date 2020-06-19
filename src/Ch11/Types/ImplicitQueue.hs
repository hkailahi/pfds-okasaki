module Ch11.Types.ImplicitQueue where

import BasicPrelude hiding (head, tail, empty)

import Ch5.Classes.Queue

data Digit a = Zero | One a | Two a a

data ImplicitQueue a =
    Shallow (Digit a)
  | Deep (Digit a) (ImplicitQueue (a, a)) (Digit a)

instance Queue ImplicitQueue where
  empty :: ImplicitQueue a
  empty = Shallow Zero

  isEmpty :: ImplicitQueue a -> Bool
  isEmpty (Shallow Zero) = True
  isEmpty _              = False

  snoc :: ImplicitQueue a -> a -> ImplicitQueue a
  snoc (Shallow Zero) y     = Shallow (One y)
  snoc (Shallow (One x)) y  = Deep (Two x y) empty Zero
  snoc (Deep f m Zero) y    = Deep f m (One y)
  snoc (Deep f m (One x)) y = Deep f (snoc m (x,y)) Zero
  snoc _ _                  = error "unimplemented"

  head :: ImplicitQueue a -> Either QueueEmpty a
  head (Shallow Zero)       = Left QueueEmpty
  head (Shallow (One x))    = Right x
  head (Deep (One x) _ _)   = Right x
  head (Deep (Two x _) _ _) = Right x
  head _                    = error "unimplemented"

  tail :: ImplicitQueue a -> Either QueueEmpty (ImplicitQueue a)
  tail (Shallow Zero)       = Left QueueEmpty
  tail (Shallow (One _))    = Right empty
  tail (Deep (Two _ y) m r) = Right $ Deep (One y) m r
  tail (Deep (One _) m r)
    | isEmpty m             = Right $ Shallow r
    | otherwise             =
        let unsafeFromRight = either (error "asljdhnfas") id
            (y,z) = unsafeFromRight $ head m
        in  Right $ Deep (Two y z) (unsafeFromRight $ tail m) r
  tail _                    = error "unimplemented"
