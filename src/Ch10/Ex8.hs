module Ch10.Ex8 where

-- import BasicPrelude

{-
Exercise 10.8
Elements in a heap frequently contain other information besides the priority.

For these kinds of elements, it is often more convenient to use heaps that separate the priority
from the rest of the element. Figure 10.8 gives an alternate signature for this kind of heap.

(a) Adapt either LazyBinomialHeap or SkewBinomialHeap to this new signature.

(b) Rewrite the Bootstrap functor as

`functor Bootstrap (PrimH : HEAPWITHINFO) : HEAPWITHINFO = ...`

You will need neither higher-order functors nor recursive structures.
-}
