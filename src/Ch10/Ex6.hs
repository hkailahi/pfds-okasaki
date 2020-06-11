module Ch10.Ex6 where

import BasicPrelude hiding (empty, (++))

import Ch10.Classes.CatenableList (CatenableList ((++)))
import Ch10.Types.CatList (CatList)
import qualified Ch5.Classes.Queue as Q

{-
Exercise 10.6:
Write a function flatten of type `[Cat a]` -> `Cat a` that catenates all the elements in a list of
catenable lists. Show that your function runs in 0(1 + e) amortized time, where e is the number of
empty catenable lists in the list.
-}

-- |NOTE - Note `foldr'`, not strict in operator `(++)`.
flatten :: (Q.Queue c, CatenableList c)
  => [c a] -> c a
flatten = foldr (++) Q.empty

flattenCatLists :: (Foldable q, Q.Queue q, (Q.Queue (CatList q)))
  => [CatList q a] -> CatList q a
flattenCatLists = flatten

{-
# Background
https://wiki.haskell.org/Fold
> One important thing to note in the presence of lazy, or normal-order evaluation, is that foldr
> will immediately return the application of f to the recursive case of folding over the rest of
> the list. Thus, if f is able to produce some part of its result without reference to the
> recursive case, and the rest of the result is never demanded, then the recursion will stop.
> This allows right folds to operate on infinite lists

# Answer
From theorem 10.1, `(++)` runs in @O(1)@. For the list of length `n`, besides the amortized cost
@O(n)@, it costs `e` in appending the empty lists. So flatten runs in `O(1+e/n) <= O(1+e)`
amortized time.

# Analysis
`flatten` with lazy (++) operator over infinite structure is O(1+e) amortized.
```
flatten (cs : xs) -> cs (++) ~(flatten xs)
```

As `CatList` is not infinite, we incur `e` to handle non-infinite case, otherwise returning
immediately on thunk from `linkAll`.
```
flatten []   -> E
flatten E:xs -> E
```
-}
