module Ch2.Ex1 where

suffixes :: [a] -> [[a]]
suffixes []          = [[]]
suffixes xs@(_:rest) = xs:(suffixes rest)
