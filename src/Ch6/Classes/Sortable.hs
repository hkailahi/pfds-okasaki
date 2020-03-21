module Ch6.Classes.Sortable where

import BasicPrelude

-- |Sortable interface from Figure 6.4
class (Ord a) => Sortable f a where

  empty :: f a
  -- ^ An empty data structure

  add :: a -> f a -> f a
  -- ^ Return the data structure with an element added

  sort :: f a -> [a]
  -- ^ Yield all the elements of the structure in a sorted list
