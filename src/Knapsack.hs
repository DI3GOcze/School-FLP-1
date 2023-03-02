-- Module containing knapsack datatypes

module Knapsack 
(   Item (..),
    Items (..),
    Knapsack (..)
) where

type Items = [Item]

-- Datatype of an item in knapsack
data Item = Item { 
    weight :: Int, 
    cost :: Int 
}

-- Datatype of knapsack
data Knapsack = Knapsack { 
    maxWeight :: Int, 
    minCost :: Int, 
    items :: Items
}

    
instance Show Knapsack where  
    show knapsack = "Knapsack: \n  Maximum weight: " ++ show (maxWeight knapsack) ++ "\n  Minimum cost: " ++ show (minCost knapsack) ++ "\n  Items: " ++ printItems (items knapsack) where
        printItems [] = ""
        printItems (item : rest) = show item ++ printItems rest

instance Show Item where  
    show item = "\n    Item: \n      Weight: " ++ show (weight item) ++ "\n      Cost: " ++ show (cost item)

    