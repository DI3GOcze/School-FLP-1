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
getBestVariant :: Knapsack -> [[Int]] -> (Int , [Int]) -> (Int , [Int])
getBestVariant _ [] bestVariant = bestVariant
getBestVariant knapsack (variant : allVariants) (bestValue, bestVariant) = do
    let (variantWeight, variantCost) = getVariantWeightAndCost (items knapsack) variant
    if variantWeight > maxWeight knapsack || variantCost < bestValue
        then getBestVariant knapsack allVariants (bestValue, bestVariant)
        else getBestVariant knapsack allVariants (variantCost, variant)

-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackBruteforce :: Knapsack -> (Bool, Solution)
solveKnapsackBruteforce knapsack
    | null (items knapsack) = (True, Solution [])
    | otherwise = do
        (True, Solution array) `debug` show weight
        where
            (weight ,array) = getBestVariant knapsack (generatePermutations (length (items knapsack))) (-1, [])

