module KnapsackBruteSolver
(
    solveKnapsackBruteforce,
    Solution (..)
) where

import Knapsack
import Debug.Trace
import Data.Foldable (foldl')

data Solution = Solution Weight Cost ItemsCombination | NoSolution deriving (Show)

type ItemsCombination = [Int]

-- TODO DELETE
debug :: c -> String -> c
debug = flip trace

-- Returns all permutations of 0 and 1 (allItemsCombinations 2 = [[0,0], [1,0], [0,1], [1,1]])
-- Params: Number of array elements
allItemsCombinations :: Int -> [ItemsCombination]
allItemsCombinations 0 = [[]]
allItemsCombinations n = [ x:xs | x <- [0,1], xs <- allItemsCombinations (n-1) ]

-- Returns weight and cost of knapsack variant passed in first parameter
-- Params: Knapsack items -> knapsack variant (eg. [0,1,0,0])
getCombinationWeightAndCost :: [Item] -> ItemsCombination -> (Weight, Cost)
getCombinationWeightAndCost items combination = foldl' (\acc (item, isIncluded) -> if isIncluded == 1 then (fst acc + weight item , snd acc + cost item) else acc) (0,0) (zip items combination)

getBestSolution :: Knapsack -> [ItemsCombination] -> Solution
getBestSolution knapsack = foldl' helper (Solution 0 0 []) where
    helper (Solution bestWeight bestCost bestCombination) combination = if combinationWeight > maxWeight knapsack || combinationCost < bestCost then Solution bestWeight bestCost bestCombination else Solution combinationWeight combinationCost combination where
        (combinationWeight, combinationCost) = getCombinationWeightAndCost (items knapsack) combination

-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackBruteforce :: Knapsack -> Solution
solveKnapsackBruteforce knapsack
    | null (items knapsack) = Solution 0 0 []
    | otherwise = getBestSolution knapsack (allItemsCombinations (length (items knapsack)))

