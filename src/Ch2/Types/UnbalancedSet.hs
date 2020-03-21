module Ch2.Types.UnbalancedSet where

import BasicPrelude hiding (Set, insert, empty)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, sized)

import Ch2.Classes.Set (Set (insert, member, empty))
import Ch2.Types.Tree (Tree (E, T))

---------------------------------------------------------------------------------------------------

newtype UnbalancedSet a = BST
  { unBST :: Tree a
  }
  deriving (Show, Eq, Functor, Foldable)

---------------------------------------------------------------------------------------------------

instance Ord a => Set UnbalancedSet a where
  empty :: UnbalancedSet a
  empty  = BST E

  insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x (BST E) = BST $ T E x E
  insert x t@(BST (T l y r))
    | x < y     = let l' = (unBST . insert x) (BST l) in BST $ T l' y r
    | x > y     = let r' = (unBST . insert x) (BST r) in BST $ T l  y r'
    | otherwise = t

  member :: a -> UnbalancedSet a -> Bool
  member _ (BST E) = False
  member x (BST (T l y r))
    | x < y     = member x (BST l)
    | x > y     = member x (BST r)
    | otherwise = x == y

---------------------------------------------------------------------------------------------------
    
instance (Arbitrary a, Ord a) => Arbitrary (UnbalancedSet a) where
  arbitrary :: Gen (UnbalancedSet a)
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n =
          insert
            <$> arbitrary
            <*> arbUSet (n-1)