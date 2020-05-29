module Ch10.Ex6 where

import BasicPrelude hiding (empty, (++))

import Ch5.Classes.Queue (Queue (empty))
import Ch10.Classes.CatenableList (CatenableList ((++)))
import Ch10.Types.CatList (CatList)

{-
Exercise 10.6:
Write a function flatten of type `[Cat a]` -> `Cat a` that catenates all the elements in a list of
catenable lists. Show that your function runs in 0(1 + e) amortized time, where e is the number of
empty catenable lists in the list.
-}

-- From theorem 10.1, `(++)` runs in @O(1)@. For the list of length `n`, besides the amortized cost
-- @O(n)@, it costs `e` in appending the empty lists. So flatten runs in `O(1+e/n) <= O(1+e)`
-- amortized time.

flatten' :: (Queue c, CatenableList c) => [c a] -> c a
flatten' = foldr (++) empty

flatten :: (Foldable q, Queue q, (Queue (CatList q))) => [CatList q a] -> CatList q a
flatten = flatten'