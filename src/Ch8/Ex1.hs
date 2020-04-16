module Ch8.Ex1 where

import BasicPrelude ()

-- Exercise 8.1
-- Extend the red-black trees of Section 3.3 with a `delete` function using these ideas. Add a
-- boolean field to the `T` constructor and maintain estimates of the numbers of valid and invalid
-- elements in the tree.
-- Assume for the purposes of these estimates that every insertion adds a new valid element and
-- that every deletion invalidates a previously valid element. Correct the estimates during
-- rebuilding. You will find Exercise 3.9 helpful in rebuilding the tree.
