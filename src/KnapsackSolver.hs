module KnapsackSolver
(
    solveKnapsackBruteforce,
) where

import Data.Array
import Knapsack

newtype Solution = Solution [Int] deriving (Show)

solveKnapsackBruteforce :: Knapsack -> (Bool, Solution)
solveKnapsackBruteforce knapsack
    | null (items knapsack) = (True, Solution [])
    | otherwise = do
        (True, Solution [])

