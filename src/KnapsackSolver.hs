module KnapsackSolver
(
    solveKnapsackBruteforce,
) where

import Data.Array
import Knapsack
import Debug.Trace

newtype Solution = Solution [Int] deriving (Show)

debug = flip trace

-- Returns weight and cost of knapsack variant passed in first parameter
-- Params: Knapsack items -> knapsack variant (eg. [0,1,0,0])
getVariantWeightAndCost :: [Item] -> [Int] -> (Int, Int)
getVariantWeightAndCost [] [] = (0, 0)
getVariantWeightAndCost (item : items) (isItemIncluded : variants) =
    -- If item is included in this variant add his cost and weight to sum
    if isItemIncluded == 1
        then do
            let (weightSum, costSum) = getVariantWeightAndCost items variants
            (weight item + weightSum , cost item + costSum)
        else getVariantWeightAndCost items variants

-- Returns all permutations of 0 and 1 (generatePermutations 2 = [[0,0], [1,0], [0,1], [1,1]])
-- Params: Number of array elements
generatePermutations :: Int -> [[Int]]
generatePermutations 0 = [[]]
generatePermutations n = [ x:xs | x <- [1,0], xs <- generatePermutations (n-1) ]

-- Returns 
-- Params: Knapsack -> All variants -> Best variant so far -> returns best variant
getBestVariant :: Knapsack -> [[Int]] -> (Int, Int, [Int])
getBestVariant _ [] = (-1, -1, [])
getBestVariant knapsack (variant : nextVariants) = do
    let (variantWeight, variantCost) = getVariantWeightAndCost (items knapsack) variant
    let (bestWeight, bestValue, bestVariant) = getBestVariant knapsack nextVariants
    if variantWeight >= maxWeight knapsack || variantCost < bestValue
        then (bestWeight, bestValue, bestVariant)
        else (variantWeight, variantCost, variant)

-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackBruteforce :: Knapsack -> (Bool, Solution)
solveKnapsackBruteforce knapsack
    | null (items knapsack) = (True, Solution [])
    | otherwise = do
        if cost < minCost knapsack then
            (False, Solution [])
        else
            (True, Solution array) `debug` (show weight ++ " " ++ show cost) where
            (weight, cost, array) = getBestVariant knapsack (generatePermutations (length (items knapsack)))
