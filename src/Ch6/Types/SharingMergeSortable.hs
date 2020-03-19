module Ch6.Types.SharingMergeSortable where

import BasicPrelude hiding (empty, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

import Ch6.Classes.Sortable (Sortable (empty, add, sort))

-- |Merge two sorted lists into a sorted list.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xs') ys@(y:ys')
  | x <= y    = x:(merge xs' ys)
  | otherwise = y:(merge xs ys')

mergeNE :: (Ord a) => NonEmpty a -> NonEmpty a -> NonEmpty a
mergeNE xs@(x :| xs') ys@(y :| ys')
  | x <= y    = x :| (merge xs' (NE.toList ys))
  | otherwise = y :| (merge (NE.toList xs) ys')

-- splitAtNE n xs splits the nonempty list xs at position n + 1
splitAtNE :: Int -> NonEmpty a -> (NonEmpty a, [a])
splitAtNE n (x :| xs) = let (xs', ys') = splitAt n xs in (x :| xs', ys')


-- |Implementation of Sortable as a list of sorted lists awaiting mergesort.
-- Each sorted list has size a power of 2 and they are stored in increasing order.
-- The Int index is the total number of elements in the data structure.
-- So there is one list of elements for each 1 bit in the element count.

data SharingMergeSortable a = SharingMergeSortable Int [NonEmpty a]
  deriving (Eq, Show)

instance (Ord a) => Sortable SharingMergeSortable a where

  empty = SharingMergeSortable 0 []

  add x (SharingMergeSortable size segments) =
    SharingMergeSortable (size + 1) (addSegment (x :| []) segments size)
    where
      addSegment :: NonEmpty a -> [NonEmpty a] -> Int -> [NonEmpty a]
      addSegment segment segmentsLeft sizeMod
        -- if the low order bit of sizeMod is 0, there is an empty slot for this segment
        | sizeMod `mod` 2 == 0 = segment:segmentsLeft
        -- otherwise we need to make a segment of double the size by merging
        | otherwise = case segmentsLeft of
            -- the front segment has the same size as segment
            headSeg:otherSegs -> addSegment (mergeNE segment headSeg) otherSegs (sizeMod `div` 2)
            -- it is not possible that sizeMod is odd but there is no front segment
            []                -> error "Forbidden by invariant of data structure"

  sort (SharingMergeSortable _ segments) = foldl merge [] $ map NE.toList segments

-- ANALYSIS of SharingMergeSortable
--
-- The idea is that when you have a full set of segments of size every power of 2 (the length
-- of the structure is 2^k - 1) then all the merges buried in the construction of those
-- segments have already been fully realized, because you had to do that in order to get the
-- space to adjoin the lower order segments. So a segment of this form should carry no debt (no
-- potential). Whereas a segment with no smaller segments (the length of the structure is
-- divisible by a large power of 2) can have a large number of unrealized merges buried in it
-- which will be realized when you try to add more elements to it, so a segment of this form
-- should carry a high potential.
--
-- Consequently, define the potential attached to a segment of size 2^k in a structure of size
-- n as: (where the constant factor C will be chosen at the end to be 2)
--
-- Vseg(n, k) = C (2^k - (n mod 2^k + 1))
--
-- Here n mod 2^k is the sum of the sizes of all the segments smaller than 2^k in the
-- structure, and the + 1 is in there to make Vseg = 0 when every segment smaller than 2^k is
-- present. As a result, if no segment smaller than 2^k is present, then Vseg(n, k) = 2^k - 1.
--
-- Then the potential of the whole structure is:
--
-- V(n) = sum(Vseg(n, k) : the 2^k bit b_k of n is not zero)
--      = sum(C (2^k - (n mod 2^k + 1)) : the 2^k bit b_k of n is not zero)
--      = C n - C sum(b_k (n mod 2^k + 1) : k >= 0)
---     = 2n - 2 sum(b_k (n mod 2^k + 1) : k >= 0)  # setting C = 2
--
-- Now fix a structure S of size n, to which we want to add an element x. Let k be the greatest
-- integer such that the k low order bits of n are 1 (k = 0 if n is even).
--
-- ::: add :::
--
-- To evaluate add x S, we will perform a single cons, and merges as necessary to fuse
-- segments. The number of such merges and their size are determined by k: if we have k low
-- order slots occupied by segments, we will merge 1 + 1, then 2 + 2, ..., then 2^(k-1) +
-- 2^(k-1). So
-- - the unshared cost of add x S is 1
-- - the shared cost of add x S is sum(2(2^j) : 0 <= j < k) = 2(2^k - 1)
-- and the complete cost of add x S is thus 2^(k+1) - 1.
--
-- The change in potential is as follows, if we let n' = n + 1 and b'_j be the jth bit of n':
--
-- V(n') - V(n) = 2 + 2 sum(b_j (n mod 2^j + 1) - b'_j (n' mod 2^j + 1) : j >= 0)
--
-- Put d(j) = b_j (n mod 2^j + 1) - b'_j (n' mod 2^j + 1). Then:
--
-- 1. In case j < k, then b_j = 1, n mod 2^j = 2^j - 1, b'_j = 0, n' mod 2^j = 0, so
--    d(j) = 2^j.
-- 2. In case j = k, then b_j = 0, n mod 2^j = 2^j - 1, b'_j = 1, n' mod 2^j = 0, so
--    d(j) = -1.
-- 3. In case j > k, then b_j = b'_j because the effect of incrementing on the bits of n stops
--    at the lowest-order 0 bit, which is b_k. Hence
--    d(j) = b_j (n mod 2^j) - b'_j (n' mod 2^j) = -b'_j.
--
-- Then we can sum this up to get
--
-- V(n') - V(n) = 2 + 2 sum(d_j : j >= 0)
--              = 2 + 2 sum(d_j : 0 <= j < k) - 2 + 2 sum(d_j : j > k)
--              = 2 + 2(2^k - 1) - 2 sum(b'_j : j >= k)
--              = 2^(k+1) - 2 B'
-- where B' is the number of 1 bits in n' (by construction n' has no 1 bit below bit k).
--
-- The amortized cost of add x S is thus the complete cost minus the potential change, that is,
--
-- 2^(k+1) - 1 - (2^(k+1) - 2 B') = 2 B' - 1
--
-- and this is O(log n') = O(log n).
--
-- ::: sort :::
--
-- To evaluate sort S, we need to pay off all the accumulated potential in S because we will
-- need to force the whole thing, and then merge the segments of S.
--
-- The accumulated potential is V(n) <= 2n = O(n).
--
-- The cost of merging is <= the cost when n = 2^k - 1 and we have to merge k successive
-- segments together. Fix k such that 2^(k-1) <= n < 2^k, then the cost of merging is
--
-- <= sum(sum(2^i : 0 <= i <= j) : 1 <= j < k)
--  = sum(2^(j+1) - 1 : 1 <= j < k) = (2^(k+1) - 4) - (k - 1) <= 4n = O(n).
--
-- Hence the total amortized cost of sort S is O(n).

-- EXERCISE 6.7

-- |Exception type for an empty structure.
data Empty = Empty
  deriving (Eq, Show)

extract :: (Ord a) => Int -> SharingMergeSortable a -> Either Empty [a]
extract k (SharingMergeSortable size segments)
  | size < k = Left Empty
  | otherwise = Right $ go k segments []
  where
    go :: (Ord a) => Int -> [NonEmpty a] -> [a] -> [a]
    go 0 _    acc = reverse acc
    go n segs acc = let (x, rest) = extractOne segs in go (n - 1) rest (x:acc)

    extractOne :: (Ord a) => [NonEmpty a] -> (a, [NonEmpty a])
    extractOne [] = error "Forbidden by invariant of data structure"
    extractOne (xs@(x :| xs') : yss) = case xs' of
      []        -> extractLoop x [xs] []           yss
      (x':xs'') -> extractLoop x [xs] [x' :| xs''] yss

    extractLoop :: (Ord a) =>
      a -> [NonEmpty a] -> [NonEmpty a] -> [NonEmpty a] -> (a, [NonEmpty a])
    extractLoop cur _       accCur [] = (cur, accCur)
    extractLoop cur accOrig accCur (xs@(x :| xs') : yss)
      | cur <= x  = extractLoop cur (xs:accOrig) (xs:accCur) yss
      | otherwise = case xs' of
          []      -> extractLoop x (xs:accOrig) accOrig yss
          x':xs'' -> extractLoop x (xs:accOrig) ((x':|xs''):accOrig) yss

-- If S has size n, then extract k S takes time O(k log n), because:
--
-- 1. go k S [] takes k times the order of time for extractOne (since reverse takes time linear
--    in its argument) since the segment list that extractOne gets is shorter each time
--
-- 2. extractOne takes constant time over extractLoop
--
-- 3. extractLoop takes constant time for each element of the segment list, and the segment
--    list has at most (binary) log n entries
