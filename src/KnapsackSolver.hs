module KnapsackSolver
(
    solveKnapsackBruteforce,
) where

import Knapsack
import Debug.Trace

newtype Solution = Solution [Int] deriving (Show)

-- TODO DELETE
debug :: c -> String -> c
debug = flip trace

filterFailedSolutions :: (Int, [Int]) -> Bool
filterFailedSolutions (cost, _) = cost >= 0

-- Returns weight and cost of knapsack variant passed in first parameter
-- Params: Knapsack items -> knapsack variant (eg. [0,1,0,0])
getVariantWeightAndCost :: [Item] -> [Int] -> (Int, Int)
getVariantWeightAndCost (item : items) (isItemIncluded : variants) =
    -- If item is included in this variant add his cost and weight to sum
    if isItemIncluded == 1
        then do
            let (weightSum, costSum) = getVariantWeightAndCost items variants
            (weight item + weightSum , cost item + costSum)
        else getVariantWeightAndCost items variants
getVariantWeightAndCost _ _ = (0, 0)

-- Returns all permutations of 0 and 1 (generatePermutations 2 = [[0,0], [1,0], [0,1], [1,1]])
-- Params: Number of array elements
generatePermutations :: Int -> [(Int, [Int])]
generatePermutations 0 = [(0, [])]
generatePermutations n = [(0, xs) | xs <- generateSublists n]
  where generateSublists 0 = [[]]
        generateSublists k = [ x:xs | x <- [0,1], xs <- generateSublists (k-1) ]

solveVariant :: Knapsack -> (Int, [Int]) -> (Int, [Int])
solveVariant _ (_, []) = (0, [])
solveVariant knapsack variant = do
    let (variantWeight, variantCost) = getVariantWeightAndCost (items knapsack) (snd variant)
    if variantWeight <= maxWeight knapsack then
        (variantCost, snd variant)
    else
        (-1, snd variant)

-- Returns 
-- Params: Knapsack -> All variants -> Best variant so far -> returns best variant (weight, cost, variant)
getBestVariant :: Knapsack -> [(Int, [Int])] -> (Int, [Int])
getBestVariant _ [] = (-1, [])
getBestVariant knapsack variants = do
    let solvedVariants = filter filterFailedSolutions (map helper variants) where
        helper = solveVariant knapsack
    if not (null solvedVariants) then
            maximum solvedVariants
    else
        (0, [])

-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackBruteforce :: Knapsack -> (Bool, Solution)
solveKnapsackBruteforce knapsack
    | null (items knapsack) = (True, Solution [])
    | otherwise = do
        if cost < minCost knapsack then
            (False, Solution [])
        else
            (True, Solution array) `debug` show cost where
            (cost, array) = getBestVariant knapsack (generatePermutations (length (items knapsack)))
