-- Funkcionální projekt do předmětu FLP
-- Autor: Jakub Kryštůfek (xkryst02)
-- Rok: 2023

-- Module containing knapsack datatypes
module KnapsackData
  ( Item (..),
    Knapsack (..),
    Weight,
    Cost,
    ItemsCombination,
    Solution(..)
  )
where

type ItemsCombination = [Int]
type Weight = Int
type Cost = Int

-- Datatype of knapsack solution 
data Solution = Solution {
  solWeight :: Weight ,
  solCost :: Cost ,
  solCombination :: ItemsCombination
}

instance Show Solution where
    show solution = "Solution [" ++ unwords (map show $ solCombination solution) ++ "]"

-- Datatype of an item in knapsack
data Item = Item {
  weight :: Weight,
  cost :: Cost
}

-- Datatype of knapsack
data Knapsack = Knapsack {
  maxWeight :: Weight,
  minCost :: Cost,
  items :: [Item]
}

instance Show Knapsack where
  show knapsack = "Knapsack {\nmaxWeight: " ++ show (maxWeight knapsack) ++ "\nminCost: " ++ show (minCost knapsack) ++ "\nitems: [" ++ printItems (items knapsack) ++ "\n]\n}"
    where
      printItems [] = ""
      printItems (item : rest) = show item ++ printItems rest

instance Show Item where
  show item = "\n\tItem {\n\tweight: " ++ show (weight item) ++ "\n\tcost: " ++ show (cost item) ++ "\n\t}"