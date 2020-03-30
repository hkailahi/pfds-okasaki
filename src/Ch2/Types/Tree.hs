module Ch2.Types.Tree where

import BasicPrelude hiding (Set, insert)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

import Ch2.Classes.Set (Set (empty, insert, member))

---------------------------------------------------------------------------------------------------

data Tree a =
    E
  | T (Tree a) a (Tree a)
  deriving stock (Show, Eq, Generic, Functor, Foldable)

---------------------------------------------------------------------------------------------------

instance (Ord a) => Set Tree a where
  empty  :: Tree a
  empty  = E

  insert :: a -> Tree a -> Tree a
  insert x E             = T E x E
  insert x s@(T lT y rT) | x < y     = T (insert x lT) y rT
                         | x > y     = T lT            y (insert x rT)
                         | otherwise = s

  member :: a -> Tree a -> Bool
  member _ E           = False
  member x (T lT y rT) | x < y     = member x lT
                       | x > y     = member x rT
                       | otherwise = True

---------------------------------------------------------------------------------------------------

-- |Builds a BST of things from a list of things
buildBST :: (Ord a) => [a] -> Tree a
buildBST = foldr insert E

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = buildBST <$> arbitrary @[a] -- NOTE Builds from a list because I'm lazy
