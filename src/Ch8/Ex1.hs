module Ch8.Ex1 where

import BasicPrelude hiding (Set, delete, empty, insert)

import Ch2.Classes.Set (Set (empty, insert, member))
-- import Ch3.Ex9
-- import Ch3.Types.RBT


-- Exercise 8.1
-- Extend the red-black trees of Section 3.3 with a `delete` function using these ideas. Add a
-- boolean field to the `T` constructor and maintain estimates of the numbers of valid and invalid
-- elements in the tree.
-- Assume for the purposes of these estimates that every insertion adds a new valid element and
-- that every deletion invalidates a previously valid element. Correct the estimates during
-- rebuilding. You will find Exercise 3.9 helpful in rebuilding the tree.

-- fromOrdList :: [a] -> RBT a
-- fromOrdList = foldl link E . foldl add []

data Color =
    R
  | B
  deriving (Eq, Show, Enum)

data RBT a =
    E
  | T Color (RBT a) (Bool, a) (RBT a)
  deriving (Eq, Show, Functor, Foldable)

data BalanceCheck a = BC
  { isUnbalanced :: Bool
  , subTrees     :: (RBT a, RBT a, RBT a, RBT a)
  , elems        :: ((Bool, a), (Bool, a), (Bool, a))
  }
  deriving (Show, Eq)

-- |Crappy indirection but all for the sake of showing rebalancing does same thing. If only haskell
-- had better pattern/or-guards.
-- Finds first nested red subtree, and perform left or right rotates to move it
-- upward
checkUnbalanced :: RBT a -> BalanceCheck a
checkUnbalanced = \case
  -- Left-left violation
  T B (T R (T R a x b) y c) z d -> BC True  (a, b, c, d) (x, y, z)
  -- Left-right violation
  T B (T R a x (T R b y c)) z d -> BC True  (a, b, c, d) (x, y, z)
  -- Right-left violation
  T B a x (T R (T R b y c) z d) -> BC True  (a, b, c, d) (x, y, z)
  -- Right-right violation
  T B a x (T R b y (T R c z d)) -> BC True  (a, b, c, d) (x, y, z)
  T _ _ x _                     -> BC False (E, E, E, E) (x, x, x)
  _ -> error "ruhroh"

-- |I think of this as a left-to-right sweep collection of subtrees @(a, b, c, d)@ and their elements
-- @(x, y, z)@ that looks two levels downward for a nested red subtree. If a nested red subtree exists,
-- invariant 2 (red children must be black) has been violated and a single O(1) rotation restores balance/invariant.
balance :: RBT a -> RBT a
balance t@(T color _ _ _) = if isUnbalanced bal
  then T R (T B a x b) y (T B c z d)
  else T color a x b
  where
    bal          = checkUnbalanced t
    (a, b, c, d) = subTrees bal
    (x, y, z)    = elems bal
balance E = E

instance Ord a => Set RBT a where
  empty :: RBT a
  empty  = E

  insert :: a -> RBT a -> RBT a
  insert x s = T B a v b
    where
      ins :: RBT a -> RBT a
      ins E                   = T R E (True, x) E
      ins (T color lT (p, y) rT)
        | x < y     = balance $ T color (ins lT) (p, y) rT
        | x > y     = balance $ T color lT       (p, y) (ins rT)
        | otherwise = T color lT (True, y) rT
      T _ a v b = ins s -- ^ guaranteed to be nonempty

  member :: a -> RBT a -> Bool
  member _ E             = False
  member x (T _ lT (p, y) rT)
    | x < y     = member x lT
    | x > y     = member x rT
    | otherwise = p

------------------------------------------------------------------------------------------

-- Identical to `Ch3.Ex9.buildSet`
buildSet :: (Set s a) => [a] -> s a
buildSet = foldl' (flip insert) empty -- equiv to foldr insert E . reverse

-- Identical to `Ch3.Ex9.fromOrdList`
fromOrdList :: (Set s a) => [a] -> s a
fromOrdList = buildSet

-- | T Color (RBT a) a (RBT a)
toOrdList :: RBT a -> [a]
toOrdList E = []
toOrdList (T _ a (p, x) b) =
  toOrdList a ++ [x | p] ++ toOrdList b

rebuild :: (Ord a) => RBT a -> RBT a
rebuild = fromOrdList . toOrdList

delete :: Ord a => a -> RBT a -> RBT a
delete _ E      = E
delete x s@T {} = rebuild $ T B a y b
  where
    del E = E
    del (T color a' (p', y') b')
      | x < y'     = T color (del a') (p', y') b'
      | y' < x     = T color a' (p', y') (del b')
      | otherwise  = T color a' (False, y') b'
    T _ a y b = del s

main :: IO ()
main = do
  print . flip (foldr delete) ("ab" :: String) $ foldr insert empty ("ebdac" :: String)
  -- => T [2,5] B (T [] B E (False,'a') E) (False,'b') (T [] R (T [] B E (True,'c') E) (True,'d') (T [] B E (True,'e') E))
  print . flip (foldr delete) ("abc" :: String) $ foldr insert empty ("ebdac" :: String)
  -- => T [0,2] B E (True,'d') (T [0,1] R E (True,'e') E)
