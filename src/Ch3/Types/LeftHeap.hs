module Ch3.Types.LeftHeap where

import BasicPrelude hiding (empty, insert)

import Ch3.Classes.Heap
  ( Heap (deleteMin, empty, findMin, insert, isEmpty, merge)
  , HeapEmpty (HeapEmpty)
  , MaxPolicy
  , MergeOrd (comparePriority)
  , MinPolicy
  , buildHeap
  )

---------------------------------------------------------------------------------------------------
-- Main API

-- |Leftist Heap w/ minimum values at head
newtype MinHeap a = MinHeap
  { unMinHeap :: LeftHeap' MinPolicy a }
  deriving stock (Show, Eq)
  deriving Heap via (LeftHeap' MinPolicy)

-- |Leftist Heap w/ maximum values at head
newtype MaxHeap a = MaxHeap
  { unMaxHeap :: LeftHeap' MaxPolicy a }
  deriving stock (Show, Eq)
  deriving Heap via (LeftHeap' MaxPolicy)

-- |(n*log n) heap insertions
-- λ> buildMinHeap elems
-- MinHeap {unMinHeap = T 2 'a'(T 1 'c' (T 1 'w' E E) E) (T 1 'b' (T 1 'd' (T 1 'e' (T 1 'z' E E) E) E) E)}
buildMinHeap :: Ord a => [a] -> MinHeap a
buildMinHeap = buildHeap @MinHeap

-- |O(n*log n) heap insertions
-- λ> buildMaxHeap elems
-- MaxHeap {unMaxHeap = T 3 'z' (T 2 'w' (T 1 'c' E E) (T 1 'a' E E)) (T 2 'e' (T 1 'd' E E) (T 1 'b' E E))}
buildMaxHeap :: Ord a => [a] -> MaxHeap a
buildMaxHeap = buildHeap @MaxHeap

---------------------------------------------------------------------------------------------------

-- λ> import qualified Ch3.Classes.Heap as Heap
-- λ> import qualified Ch3.Types.LeftHeap as LH
-- λ> elems = ['c', 'w', 'a', 'z', 'e', 'd', 'b']
-- λ> Heap.findMin $ foldr Heap.insert LH.E elems
-- 'a'

-- λ> foldl' (flip Heap.insert) LH.E elems
-- T 2 'a' (T 1 'c' (T 1 'w' E E) E) (T 1 'b' (T 1 'd' (T 1 'e' (T 1 'z' E E) E) E) E)
-- λ> scanl' (flip Heap.insert) LH.E elems
-- [ E
-- , T 1 'c' E E
-- , T 1 'c' (T 1 'w' E E) E
-- , T 1 'a' (T 1 'c' (T 1 'w' E E) E) E
-- , T 2 'a' (T 1 'c' (T 1 'w' E E) E) (T 1 'z' E E)
-- , T 2 'a' (T 1 'c' (T 1 'w' E E) E) (T 1 'e' (T 1 'z' E E) E)
-- , T 2 'a' (T 1 'c' (T 1 'w' E E) E) (T 1 'd' (T 1 'e' (T 1 'z' E E) E) E)
-- , T 2 'a' (T 1 'c' (T 1 'w' E E) E) (T 1 'b' (T 1 'd' (T 1 'e' (T 1 'z' E E) E) E) E)
-- ]
--
-- Ex. Rank:Elem, with empties (0:_) omitted
--     ->       ->     1:a ->      2:a     ->      2:a     ->      2:a     ->      2:a     |
--     ->       ->     /   ->     /   \    ->     /   \    ->     /   \    ->     /   \    |
-- 1:c ->   1:c ->   1:c   ->   1:c    1:z ->   1:c    1:e ->   1:c    1:d ->   1:c    1:b |
--     ->   /   ->   /     ->   /           ->   /      /  ->   /      /   ->   /      /   |
--     -> 1:w   -> 1:w     -> 1:w           -> 1:w    1:z  -> 1:w    1:e   -> 1:w    1:d   |
--     ->       ->         ->               ->             ->        /     ->        /     |
--     ->       ->         ->               ->             ->      1:z     ->      1:e     |
--     ->       ->         ->               ->             ->              ->      /       |
--     ->       ->         ->               ->             ->              ->    1:z       |

-- λ> foldr Heap.insert LH.E elems
-- T 2 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' (T 1 'z' E E) E)) (T 1 'c' (T 1 'w' E E) E)
-- λ> reverse $ scanr Heap.insert LH.E elems
-- [ E
-- , T 1 'b' E E
-- , T 1 'b' (T 1 'd' E E) E
-- , T 2 'b' (T 1 'd' E E) (T 1 'e' E E)
-- , T 2 'b' (T 1 'd' E E) (T 1 'e' (T 1 'z' E E) E)
-- , T 1 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' (T 1 'z' E E) E)) E
-- , T 2 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' (T 1 'z' E E) E)) (T 1 'w' E E)
-- , T 2 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' (T 1 'z' E E) E)) (T 1 'c' (T 1 'w' E E) E)
-- ]
--
--  Ex. Rank:Elem, with empties (0:_) omitted
--     ->       ->         ->         ->      1:a  ->      2:a     ->      2:a     |
--     ->       ->         ->         ->     /     ->     /   \    ->     /   \    |
-- 1:b ->   1:b ->   2:b   ->   2:b   ->   2:b     ->   2:b    1:w ->   2:b    1:c |
--     ->   /   ->   / \   ->   / \   ->   / \     ->   / \        ->   / \    /   |
--     -> 1:d   -> 1:d 1:e -> 1:d 1:e -> 1:d 1:e   -> 1:d 1:e      -> 1:d 1:e 1:w  |
--     ->       ->         ->     /   ->      /    ->      /       ->      /       |
--     ->       ->         ->    1:z  ->     1:z   ->     1:z      ->     1:z      |

---------------------------------------------------------------------------------------------------

-- |Int-ranked min leftist heap yo
data LeftHeap' prio a =
    E
  | T Int a (LeftHeap' prio a) (LeftHeap' prio a)
  deriving (Show, Eq, Functor, Foldable)
type LeftHeap a = LeftHeap' MinPolicy a

---------------------------------------------------------------------------------------------------

instance (MergeOrd prio) => Heap (LeftHeap' prio) where
  empty :: LeftHeap' prio a
  empty = E

  -- |O(1) empty check
  isEmpty :: LeftHeap' prio a -> Bool
  isEmpty = \case
    E -> True
    _ -> False

  -- |O(log n) insert
  insert :: (Ord a) => a -> LeftHeap' prio a -> LeftHeap' prio a
  insert x = merge (T 1 x E E)

  -- |O(log n) merge
  -- Two leftist heaps can be merged by merging their right spines as you would merge two sorted
  -- lists, and then swapping the children of nodes along this path as necessary to restore the
  -- leftist property
  merge :: (Ord a) => LeftHeap' prio a -> LeftHeap' prio a -> LeftHeap' prio a
  merge h1 E  = h1
  merge E  h2 = h2
  merge h1@(T _ x lT1 rT1) h2@(T _ y lT2 rT2)
    | comparePriority @prio x y = makeT x lT1 $ merge rT1 h2  -- (<) if min heap, (>=) if max heap
    | otherwise                 = makeT y lT2 $ merge h1  rT2
    where
      rank :: LeftHeap' prio a -> Int
      rank E           = 0
      rank (T r _ _ _) = r
      makeT :: a -> LeftHeap' prio a -> LeftHeap' prio a -> LeftHeap' prio a
      makeT e lT rT
        | rank lT >= rank rT = T (rank lT + 1) e lT rT
        | otherwise        = T (rank rT + 1) e rT lT

  -- |partial, O(1) find min since located at head
  findMin :: LeftHeap' prio a -> Either HeapEmpty a
  findMin (T _ x _ _) = Right $ x
  findMin _           = Left HeapEmpty

  -- |partial, O(log n) delete min since located at head
  deleteMin :: (Ord a) => LeftHeap' prio a -> Either HeapEmpty (LeftHeap' prio a)
  deleteMin (T _ _ lT rT) = Right $ merge lT rT
  deleteMin _             = Left HeapEmpty
