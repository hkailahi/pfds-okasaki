module Ch6.Ex1 where

import BasicPrelude

-- Exercise:
-- Draw an execution trace for the following set of operations. Annotate each node in the trace
-- with the number of logical futures at that node.

-- | Append an element to the end of a list, takes /O(n)/ time.
--
-- > snoc "tes" 't' == "test"
-- > \xs x -> unsnoc (snoc xs x) == Just (xs,x)
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

a, b, c, d, e, f, g :: [Int]
a = snoc mempty 0 -- [0]
b = snoc a 1      -- [0,1]
c = tail b        -- [1]
d = snoc b 2      -- [0,1,2]
e = c ++ d        -- [1,0,1,2]
f = tail c        -- []
g = snoc d 3      -- [0,1,2,3]

{-
6.1

=====================================
| Execution Trace | Logical Futures |
|-----------------|-----------------|
|      empty      |        4        |
|        |        |        |        |
|        a        |        4        |
|        |        |        |        |
|        b        |        4        |
|       / \       |       / \       |
|      c   d      |      2   2      |
|     / \ / \     |     / \ / \     |
|    f   e   g    |    1   1   1    |
=====================================

Despite there only being 3 possible outcomes (e, f, g), since there are two ways of getting to `e`,
early nodes have 4 logical futures.

============================================
|              Logical Futures             |
|               of a / empty               |
|------------------------------------------|
| History  | History | History  |  History |
|  of f    | #1 of e | #2 of e  |   of g   |
|   (f^)   |       (e^)         |   (g^)   |
|------------------------------------------|
|    empty |  empty  |  empty   | empty    |
|      |   |    |    |    |     |   |      |
|      a   |    a    |    a     |   a      |
|      |   |    |    |    |     |   |      |
|      b   |    b    |    b     |   b      |
|     /    |   /     |     \    |    \     |
|    c     |  c      |      d   |     d    |
|   /      |   \     |     /    |      \   |
|  f       |    e    |    e     |       g  |
============================================

-}
