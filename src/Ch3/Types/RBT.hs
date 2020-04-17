module Ch3.Types.RBT where

import BasicPrelude hiding (Set, empty, insert)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, sized)

import Ch2.Classes.Set (Set (empty, insert, member))

---------------------------------------------------------------------------------------------------

data Color =
    R
  | B
  deriving (Eq, Show, Enum)

data RedBlackSet a =
    E
  | T Color (RedBlackSet a) a (RedBlackSet a)
  deriving (Eq, Show, Functor, Foldable)

data BalanceCheck a = BC
  { isUnbalanced :: Bool
  , subTrees     :: (RedBlackSet a, RedBlackSet a, RedBlackSet a, RedBlackSet a)
  , elems        :: (a, a, a)
  }
  deriving (Show, Eq)

-- |Crappy indirection but all for the sake of showing rebalancing does same thing. If only haskell
-- had better pattern/or-guards.
-- Finds first nested red subtree, and perform left or right rotates to move it
-- upward
checkUnbalanced :: Color -> RedBlackSet a -> a -> RedBlackSet a -> BalanceCheck a
-- Left-left violation
checkUnbalanced B (T R (T R a x b) y c) z d = BC True  (a, b, c, d) (x, y, z)
-- Left-right violation
checkUnbalanced B (T R a x (T R b y c)) z d = BC True  (a, b, c, d) (x, y, z)
-- Right-left violation
checkUnbalanced B a x (T R (T R b y c) z d) = BC True  (a, b, c, d) (x, y, z)
-- Right-right violation
checkUnbalanced B a x (T R b y (T R c z d)) = BC True  (a, b, c, d) (x, y, z)
checkUnbalanced _ _ x _ = BC False (E, E, E, E) (x, x, x)

-- |I think of this as a left-to-right sweep collection of subtrees @(a, b, c, d)@ and their elements
-- @(x, y, z)@ that looks two levels downward for a nested red subtree. If a nested red subtree exists,
-- invariant 2 (red children must be black) has been violated and a single O(1) rotation restores balance/invariant.
balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance color lT e rT = if isUnbalanced bal
  then T R (T B a x b) y (T B c z d)
  else T color a x b
  where 
    bal          = checkUnbalanced color lT e rT
    (a, b, c, d) = subTrees bal
    (x, y, z)    = elems bal

instance Ord a => Set RedBlackSet a where
  empty :: RedBlackSet a
  empty  = E

  insert :: a -> RedBlackSet a -> RedBlackSet a
  insert x s = T B a v b
    where
      ins :: RedBlackSet a -> RedBlackSet a
      ins E                   = T R E x E
      ins t@(T color lT y rT)
        | x < y     = balance color (ins lT) y rT
        | x > y     = balance color lT       y (ins rT)
        | otherwise = t
      T _ a v b = ins s -- ^ guaranteed to be nonempty

  member :: a -> RedBlackSet a -> Bool
  member _ E             = False
  member x (T _ lT y rT)
    | x < y     = member x lT
    | x > y     = member x rT
    | otherwise = True

---------------------------------------------------------------------------------------------------

instance (Arbitrary a, Ord a) => Arbitrary (RedBlackSet a) where
  arbitrary :: Gen (RedBlackSet a)
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n =
          insert
            <$> arbitrary
            <*> arbUSet (n-1)
