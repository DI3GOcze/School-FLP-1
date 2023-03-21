module KnapsackBruteSolver
  ( solveKnapsackBruteforce,
    Solution (..),
  )
where

import Data.Foldable (foldl')
import Knapsack
  ( Cost,
    Item (cost, weight),
    Knapsack (items, maxWeight, minCost),
    Weight,
  )

type ItemsCombination = [Int]

data Solution = Solution
  { solWeight :: Weight,
    solCost :: Cost,
    solCombination :: ItemsCombination
  }

instance Show Solution where
  show :: Solution -> String
  show solution = "Solution " ++ show (solCombination solution)

-- Returns all permutations of 0 and 1 (allItemsCombinations 2 = [[0,0], [1,0], [0,1], [1,1]])
-- Params: Number of array elements
allItemsCombinations :: Int -> [ItemsCombination]
allItemsCombinations 0 = [[]]
allItemsCombinations n = [x : xs | x <- [0, 1], xs <- allItemsCombinations (n - 1)]

-- Returns weight and cost of knapsack variant passed in first parameter
-- Params: Knapsack items -> knapsack variant (eg. [0,1,0,0])
getCombinationWeightAndCost :: [Item] -> ItemsCombination -> (Weight, Cost)
getCombinationWeightAndCost items combination = foldl' (\acc (item, isIncluded) -> if isIncluded == 1 then (fst acc + weight item, snd acc + cost item) else acc) (0, 0) (zip items combination)

-- Returns best solution of passed knapsack problem
getBestSolution :: Knapsack -> [ItemsCombination] -> Solution
getBestSolution knapsack combinations
  | null (items knapsack) = Solution 0 0 []
  | otherwise = foldl' helper (Solution 0 0 []) combinations
  where
    helper :: Solution -> ItemsCombination -> Solution
    helper bestSolution combination = if combinationWeight > maxWeight knapsack || combinationCost < solCost bestSolution then bestSolution else Solution combinationWeight combinationCost combination
      where
        (combinationWeight, combinationCost) = getCombinationWeightAndCost (items knapsack) combination

-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackBruteforce :: Knapsack -> Maybe Solution
solveKnapsackBruteforce knapsack = if solCost bestSolution >= minCost knapsack then Just bestSolution else Nothing
  where
    bestSolution = getBestSolution knapsack (allItemsCombinations (length (items knapsack)))