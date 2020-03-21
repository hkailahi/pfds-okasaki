module Ch2.Classes.Set where

import BasicPrelude hiding (Set, insert)

class Set (s :: * -> *) (e :: *) where
  empty  :: s e
  insert :: e -> s e -> s e
  member :: e -> s e -> Bool
