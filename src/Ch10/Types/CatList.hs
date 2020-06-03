{-# LANGUAGE UndecidableInstances #-}

module Ch10.Types.CatList where

import BasicPrelude hiding (head, tail, (++))

import Ch10.Classes.CatenableList
import Ch5.Classes.Queue (Queue (..))
import qualified Ch5.Classes.Queue as Q

-- |Isomorphic to `Cofree :+: ()` (can be an unlabelled empty)
data CatList q a =
    E
  | C a (q (CatList q a))
deriving instance (Show a, Show (q (CatList q a))) => Show (CatList q a)
deriving instance (Eq a, Eq (q (CatList q a))) => Eq (CatList q a)

link :: (Queue q) => CatList q a -> CatList q a -> CatList q a
link (C x q) s = C x (Q.snoc q s)
link E _       = error "nfasoinoaisndfoins"

-- |Since catenation is associative, we can link the children in any order we desire.
-- However, a little thought reveals that linking the children from right to left, as illustrated
-- in Figure 10.5, will duplicate the least work on subsequent calls to tail.
-- `linkAll` is `foldr1`
-- linkAll :: (Queue q) => CatList q a -> a -> CatList q a
linkAll :: (Queue f, Queue q) => f (CatList q a) -> CatList q a
linkAll queue =
  let !t = (\case Right x -> x; _ -> error "asdf") $ Q.head queue
      q' = (\case Right x -> x; _ -> error "asdf") $ Q.tail queue
  in if Q.isEmpty q'
    then t
    -- `linkAll q'` intentionally suspended
    else link t $ linkAll q'

instance (Foldable q, Queue q) => CatenableList (CatList q) where
  empty :: CatList q a
  empty = E

  -- |O(1)
  isEmpty :: CatList q a -> Bool
  isEmpty E = True
  isEmpty _ = False

  -- |Amortized O(1)
  (++) :: CatList q a -> CatList q a -> CatList q a
  xs ++ E  = xs
  E ++ xs  = xs
  xs ++ ys = link xs ys

  -- |Amortized O(1)
  cons :: a -> CatList q a -> CatList q a
  cons x xs = C x Q.empty ++ xs

  -- |Amortized O(1)
  snoc :: CatList q a -> a -> CatList q a
  snoc xs x = xs ++ C x Q.empty

  -- |O(1)
  head :: CatList q a -> Either CatenableListEmpty a
  head E       = Left CatenableListEmpty
  head (C x _) = Right x

  -- |Amortized O(1)
  tail :: CatList q a -> Either CatenableListEmpty (CatList q a)
  tail E = Left CatenableListEmpty
  tail (C _ q) =
    if Q.isEmpty q then Right E else Right $ linkAll q
