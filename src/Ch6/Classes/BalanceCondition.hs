{-# LANGUAGE AllowAmbiguousTypes #-}

module Ch6.Classes.BalanceCondition where

import BasicPrelude

---------------------------------------------------------------------------------------------------
-- Parematerizing the balance invariant

class BalanceCondition c where
  needsRebalance :: Int -> Int -> Bool

-- |The `|front| <= |rear|` queue invariant. Rebalances are needed to maintain this property
data F_lte_R
-- |The `2 * |front| <= |rear|` queue invariant. Rebalances are needed to maintain this property
data TwoF_lte_R

instance BalanceCondition F_lte_R where
  needsRebalance :: Int -> Int -> Bool
  needsRebalance lenF lenR = not $ lenF >= lenR

instance BalanceCondition TwoF_lte_R where
  needsRebalance :: Int -> Int -> Bool
  needsRebalance lenF lenR = not $ 2 * lenF >= lenR
