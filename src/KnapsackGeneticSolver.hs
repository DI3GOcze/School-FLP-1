module KnapsackGeneticSolver
  ( solveKnapsackOptimized,
    Solution (..),
  )
where

import System.Random
import Debug.Trace
import Data.Foldable (foldl')
import Knapsack
  ( Cost,
    Item (cost, weight),
    Knapsack (items, maxWeight, minCost),
    Weight,
  )

debug = flip trace

-- Numer of initial population
initialLength :: Int
initialLength = 6

type ItemsCombination = [Int]

data Solution = Solution
  { solWeight :: Weight,
    solCost :: Cost,
    solCombination :: ItemsCombination
  }

instance Show Solution where
  show :: Solution -> String
  show solution = "Solution " ++ show (solCombination solution)


-- Solve knapsack problem with brute force (trying all permutations of knapsack problem)
solveKnapsackOptimized :: Knapsack -> StdGen -> Maybe Solution
solveKnapsackOptimized knapsack gen = do
  let a = generateInitialGeneration initialLength (length $ items knapsack) gen
  let b = generateInitialGeneration initialLength (length $ items knapsack)
  Just (Solution 1 1 (fst (randomItemsCombination 5 gen))) `debug` show a


randomItemsCombination :: Int -> StdGen -> (ItemsCombination, StdGen)
randomItemsCombination 0 gen = ([], gen)
randomItemsCombination n gen =
    let (value, newGen) = randomR (0,1) gen
        (restOfList, finalGen) = randomItemsCombination (n-1) newGen
    in  (value:restOfList, finalGen)

generateInitialGeneration :: Int -> Int -> StdGen -> ([ItemsCombination], StdGen)
generateInitialGeneration 0 _ gen = ([], gen)
generateInitialGeneration n itemsCount gen =
    let (value, newGen) = randomItemsCombination itemsCount gen
        (restOfList, finalGen) = generateInitialGeneration (n-1) itemsCount newGen
    in  (value:restOfList, finalGen)

-- Returns fitness coeficient of knapsack combination
getCombinationFitness :: Knapsack -> ItemsCombination -> Cost
getCombinationFitness knapsack combination =
  let (combinationWeight, combinationCost) = foldl' (\acc (item, isIncluded) -> (isIncluded * fst acc + weight item, isIncluded * snd acc + cost item)) (0, 0) (zip (items knapsack) combination)
  in if combinationWeight > maxWeight knapsack then 0 else combinationCost

-- -- Pick two elements from array and return them and the rest of the array
-- pickTwoRandomUnique :: [a] -> StdGen -> ((a,a), [a])
-- pickTwoRandomUnique lst = 
--   let first =  

-- Deletes array element on given index
deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex i xs = let (ys, zs) = splitAt i xs in ys ++ tail zs


-- -- Returns best solution of passed knapsack problem
-- getBestSolution :: Knapsack -> [ItemsCombination] -> Solution
-- getBestSolution knapsack combinations
--   | null (items knapsack) = Solution 0 0 []
--   | otherwise = foldl' helper (Solution 0 0 []) combinations
--   where
--     helper :: Solution -> ItemsCombination -> Solution
--     helper bestSolution combination = if combinationWeight > maxWeight knapsack || combinationCost < solCost bestSolution then bestSolution else Solution combinationWeight combinationCost combination
--       where
--         (combinationWeight, combinationCost) = getCombinationWeightAndCost (items knapsack) combination
