module Ch2.Ex1 where

-- Exercise 2.1 Write a function suffixes of type `[a] -> [[a]]` that takes a list `xs` and returns a list of
-- all the suffixes of xs in decreasing order of length.
-- For example:
-- >>> suffixes [1,2,3,4]
-- [[1,2,3,4], [2,3,4], [3,4], [4], []]
-- Show that the resulting list of suffixes can be generated in O(n) time and represented in O(n) space.

suffixes :: [a] -> [[a]]
suffixes []       = [[]]
suffixes l@(_:xs) = l : suffixes xs

{-

O(n) time:
0. suffixes [1,2,3,4]
 - l0 = 1 : 2 : 3 : 4 : []
 - xs =     2 : 3 : 4 : []
1. l0 : suffixes [2,3,4]
 - l1 = 2 : 3 : 4 : []
 - xs =     3 : 4 : []
2. l0 : l1 : suffixes [3,4]
 - l2 = 3 : 4 : []
 - xs =     4 : []
3. l0 : l1 : l2 : suffixes [4]
 - l3 = 4 : []
 - xs =     []
4. l0 : l1 : l2 : l3 : suffixes []
 - l4 = []

O(n) space:
 Result = [ 1 : 2 : 3 : 4 : []
          ,     2 : 3 : 4 : []
          ,         3 : 4 : []
          ,             4 : []
          ,                 []
          ]
Stored as [ 1 : v
          ,     2 : v
          ,         3 : v
          ,             4 : v
          ,                 []
          ]
-}
