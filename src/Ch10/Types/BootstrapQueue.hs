module Ch10.Types.BootstrapQueue where

import BasicPrelude hiding (empty, head, tail)

import Ch5.Classes.Queue (Queue (..), QueueEmpty (..))

-- |Data type for queues which represent their front as a front list and a queue of
-- future lists having been reversed from the rear, awaiting promotion to be the new front.
-- NB. This does the thing you're not supposed to do where you have a raw record type
-- wrapped in one constructor of an algebraic data type, and then the accessors for that
-- record type are partial functions. The functions are not used in the code below --
-- the record fields are only ever used for pattern matching -- but I shouldn't have
-- written it this way. What you're supposed to do is put a Detail type as the argument
-- of the constructor.

data BootstrapQueue a =
    BQEmpty
  | BQ { bqLenFronts    :: Integer
       , bqFront        :: [a]
       , bqFutureFronts :: BootstrapQueue [a]
       , bqLenRear      :: Integer
       , bqRear         :: [a]
       }
  deriving (Eq, Show)

-- |Rebalancing helper. Doesn't use the invariant stuff from Ch6.Types.BankersQueue, but could.
-- This is `checkQ` in Figure 10.2.
rebalance :: BootstrapQueue a -> BootstrapQueue a
rebalance BQEmpty = BQEmpty
rebalance q@BQ{..}
  | bqLenRear <= bqLenFronts = ensureFront q
  | otherwise =
      ensureFront $ BQ { bqLenFronts    = bqLenFronts + bqLenRear
                       , bqFront        = bqFront
                       , bqFutureFronts = snoc bqFutureFronts $ reverse bqRear
                       , bqLenRear      = 0
                       , bqRear         = []
                       }
  where
    -- Having verified that the front is >= the rear, ensure that bqFront is nonempty if
    -- possible, or else the queue as a whole is replaced with empty. This is `checkF` in
    -- Figure 10.2.
    ensureFront :: BootstrapQueue a -> BootstrapQueue a
    ensureFront BQEmpty = BQEmpty
    -- This case need not check bqRear because we know bqLenRear <= bqLenFronts
    ensureFront BQ { bqFront = [], bqFutureFronts = BQEmpty } = BQEmpty
    -- Otherwise, we know bqFutureFronts is nonempty, but the type doesn't reveal that
    -- to head and tail, so we get an invariant error case. Interestingly if you instead
    -- pattern match to show that bqFutureFronts is the BQ constructor, the result does
    -- not type check.
    ensureFront q'@BQ { bqFront = [], bqFutureFronts = bqFutureFronts' } =
      case (head bqFutureFronts', tail bqFutureFronts') of
        (Right newFront, Right newFuture)
          -> q' { bqFront = newFront, bqFutureFronts = newFuture }
        _ -> error "forbidden by previous check that bqFutureFronts is not empty"
    ensureFront q' = q'

instance Queue BootstrapQueue where

  empty = BQEmpty

  isEmpty BQEmpty = True
  isEmpty _       = False

  snoc BQEmpty  x = BQ 1 [x] BQEmpty 0 []
  snoc q@BQ{..} x = rebalance q { bqLenRear = bqLenRear + 1
                                , bqRear    = x:bqRear
                                }

  head BQEmpty              = Left QueueEmpty
  head BQ { bqFront = [] }  = error "forbidden by invariant of rebalance/ensureFront"
  head BQ { bqFront = x:_ } = Right x

  tail BQEmpty                     = Left QueueEmpty
  tail BQ { bqFront = [] }         = error "forbidden by invariant of rebalance/ensureFront"
  tail q@BQ { bqFront = _:xs, .. } = Right $ rebalance q { bqLenFronts = bqLenFronts - 1
                                                         , bqFront     = xs
                                                         }

{- |EXERCISE 10.3. Show that in `tail $ snoc q x`, the calls to `tail` and `snoc` cannot both
incur a recursive `snoc`.

This is because if `snoc q x` incurs a recursive `snoc`, after that `bqRear` is empty, so
the recursive `snoc` case of `rebalance` cannot trigger on the subsequent `tail`.
-}

{- |EXERCISE 10.4 skipping this because Haskell has polymorphic recursion. -}

{- |EXERCISE 10.5 skipping this one too because this is literally what type classes do. -}
