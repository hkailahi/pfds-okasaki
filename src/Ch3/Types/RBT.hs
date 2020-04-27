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

balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b                 = T color a x b

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
      T _ a v b = ins s -- guaranteed to be nonempty

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
