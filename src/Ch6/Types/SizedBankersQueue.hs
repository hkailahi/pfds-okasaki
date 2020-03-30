{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Ch6.Types.SizedBankersQueue where

-- import BasicPrelude hiding (replicate)
-- import Data.Bifunctor (Bifunctor (bimap))
-- import Data.Text (replicate)

-- import qualified "fin" Data.Fin as F
-- import qualified "fin" Data.Type.Nat as N
-- import           "fin" Data.Nat (Nat)
-- import "vec" Data.Vec.Lazy (Vec (VNil, (:::)))

-- import Ch5.Classes.Queue (Queue (empty, head, isEmpty, snoc, tail), QueueEmpty (QueueEmpty))
-- import Ch6.Classes.BalanceCondition (BalanceCondition (needsRebalance), F_lte_R)

-- ---------------------------------------------------------------------------------------------------

-- -- NOTE Per "vec", `F.Nat` comes from "fin" lib, not `GHC.TypeLits`

-- ---------------------------------------------------------------------------------------------------
-- -- Types and Accessors

-- -- From `Data.Vec.Lazy`:
-- -- @
-- --     -- | Vector, i.e. length-indexed list.
-- --     data Vec (n :: Nat) a where
-- --         VNil  :: Vec 'Z a
-- --         (:::) :: a -> Vec n a -> Vec ('S n) a
-- --       deriving (Typeable)
-- -- @

-- data SizedBankersQueue' (f :: N.Nat) (r :: N.Nat) invariant a = SizedBankersQueue
--   { front :: Vec f a
--   , rear  :: Vec r a
--   } deriving (Show, Eq, Functor, Foldable)
-- type SizedBankersQueue f r a = SizedBankersQueue' f r F_lte_R a

-- -- {-# COMPLETE SBQ #-}
-- -- |Because I wanted named fields, but actually requiring field names is annoying because in most
-- -- cases they're just noise. With explicit exports, I can decide to not export this pattern if I'm
-- -- worried about user's of my `SizedBankersQueue` libary misusing it.
-- pattern SBQ :: Vec f a -> Vec r a -> SizedBankersQueue' f r inv a
-- pattern SBQ front rear = SizedBankersQueue
--   { front = front
--   , rear  = rear
--   }

-- pattern EmptySBQ :: SizedBankersQueue' N.Nat0 N.Nat0 inv a
-- pattern EmptySBQ = SizedBankersQueue
--   { front = VNil
--   , rear  = VNil
--   }

-- ---------------------------------------------------------------------------------------------------

-- instance Queue (SizedBankersQueue' (f :: N.Nat) (r :: N.Nat) inv) where
--   -- |O(1)
--   -- FIXME WHHHHHHYYYYYYYYSDFOISNBJSNDOIUBSNDVSV :(
--   -- empty :: (f ~ N.Nat0, r ~ N.Nat0) => SizedBankersQueue' f r inv a
--   empty :: SizedBankersQueue' N.Nat0 N.Nat0 inv a
--   empty = EmptySBQ

--   -- |O(1)
--   isEmpty :: SizedBankersQueue' f r inv a -> Bool
--   isEmpty (_ :: SizedBankersQueue' N.Nat0 r inv a) = True
--    -- ^ TODO try harder to get "type variables in patterns" working, if that makes sense here
--   isEmpty _                                        = False

--   -- |O(1)
--   head :: SizedBankersQueue' f r inv a -> Either QueueEmpty a
--   head (SBQ ((:::) x _) _) = Right x
--   head _                   = Left QueueEmpty

--   -- |Amortized O(1)
--   -- simple snoc (no rebalance)
--   snoc ::
--        SizedBankersQueue' f r                 inv a
--     ->                                            a
--     -> SizedBankersQueue' f (N.Plus r N.Nat1) inv a
--   snoc (SBQ f r) x = SBQ f (x (:::) r)

--   -- |Amortized O(1)
--   tail ::                SizedBankersQueue' (N.Plus finalF N.Nat0)  r inv a
--    -> Either QueueEmpty (SizedBankersQueue' finalF                  r inv a)
--   tail (SBQ ((:::) _ xs) r) = Right $ SBQ xs r
--   tail _                    = Left QueueEmpty

--   -- |Amortized O(1)
--   -- Only when front is smaller than rear does the O(r) frontload happen
--   -- snoc :: SizedBankersQueue' f r inv a -> a -> SizedBankersQueue' f r inv a
--   -- snoc (SBQ lenF f lenR r) x = frontLoad $ SBQ lenF f (lenR + 1) (x:r)
--   -- |Amortized O(1)
--   -- Only when front is smaller than rear does the O(r) frontload happen
--   -- tail ::                SizedBankersQueue' (N.Plus finalF N.Nat0)  r inv a
--   --  -> Either QueueEmpty (SizedBankersQueue' f                       r inv a)
--   -- tail (SBQ ((:::) _ xs) r) = Right . frontLoad $ SBQ (lenF - 1) xs lenR r
--   -- tail _                    = Left QueueEmpty
