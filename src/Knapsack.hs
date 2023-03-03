-- Module containing knapsack datatypes

module Knapsack 
(   Item (..),
    Knapsack (..),
    Weight,
    Cost
) where

type Weight = Int
type Cost = Int

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
    show knapsack = "Knapsack: \n  Maximum weight: " ++ show (maxWeight knapsack) ++ "\n  Minimum cost: " ++ show (minCost knapsack) ++ "\n  Items: " ++ printItems (items knapsack) where
        printItems [] = ""
        printItems (item : rest) = show item ++ printItems rest

instance Show Item where  
    show item = "\n    Item: \n      Weight: " ++ show (weight item) ++ "\n      Cost: " ++ show (cost item)

    