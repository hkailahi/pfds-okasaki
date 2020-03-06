#!/usr/bin/env stack
{- stack
    --resolver lts-14.21
    exec ghci
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Ch6.DebugEx1 where

-- import BasicPrelude hiding (unlines)
-- import Prelude (unlines)

-- snoc :: [a] -> a -> [a]
-- snoc xs x = xs ++ [x]

-- a, b, c, d, e, f, g :: [Int]
-- a = snoc mempty 0 -- [0]
-- b = snoc a 1      -- [0,1]
-- c = tail b        -- [1]
-- d = snoc b 2      -- [0,1,2]
-- e = c ++ d        -- [1,0,1,2]
-- f = tail c        -- []
-- g = snoc d 3      -- [0,1,2,3]

-- purc :: Applicative f => a -> a -> f a
-- purc a b = pure $ const a b

-- main :: IO [[Int]]
-- main = do
--   v <- pure a        -- [0]
--   v <- purc b v      -- [0,1]
--   v <- purc c v      -- [1]
--   v <- purc d v      -- [0,1,2]
--   v <- purc e v      -- [1,0,1,2]
--   v <- purc f v      -- []
--   v <- purc g v      -- []
--   _ <- pure v        -- [0,1,2,3]
--   _ <- pure [1..10]  -- final breakpoint step
--   purc [e, f, g] [v]

-- {-
-- From https://old.reddit.com/r/haskell/comments/dv0gye/it_turns_out_ghci_is_a_wellfeatured_debugger_too/

-- $ stack ghci src/Ch6/Ex1.hs --ghc-options -Wno-type-defaults,-Wno-missing-signatures,-Wno-name-shadowing
-- -- <name> <expr, ie the fn in this file>
-- :def debug debug
-- -- Break on a line num
-- :break 30
-- :break 31
-- :break 32
-- :break 33
-- :break 34
-- :break 35
-- :break 36
-- :break 37
-- -- :break 38
-- -- Show breakpoints set
-- :show breaks
-- -- Apply that `:debug` (not `debug`) on the 0th breakpoint
-- :set stop 0 :debug
-- :set stop 1 :debug
-- :set stop 2 :debug
-- :set stop 3 :debug
-- :set stop 4 :debug
-- :set stop 5 :debug
-- :set stop 6 :debug
-- :set stop 7 :debug
-- -- :set stop 8 :debug
-- -- Eval, keeping a history
-- :trace main
-- -- View the history
-- :history
-- -- Go back in history 5 steps
-- :back 5
-- -- Check out what `v` was at that time
-- v
-- -- Continue
-- :cont

-- -}
-- debug :: Monad m => p -> m String
-- debug _ = return $ unlines [
--   "\"----\""
--   , "v"
--   , ":cmd if (v==[1..10]) then return \"\" else return \":continue\""
--   ]
