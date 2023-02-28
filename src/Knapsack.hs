-- Module containing knapsack datatypes

module Knapsack 
(   Item (..),
    Knapsack (..),
) where

-- Datatype of an item in knapsack
data Item = Item { 
    weight :: Int, 
    cost :: Int 
} deriving (Show)

-- Datatype of knapsack
data Knapsack = Knapsack { 
    maxWeight :: Int, 
    minCost :: Int, 
    items :: [Item] 
} deriving (Show)