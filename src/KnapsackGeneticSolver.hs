-- Funkcionální projekt do předmětu FLP
-- Autor: Jakub Kryštůfek (xkryst02)
-- Rok: 2023

module KnapsackGeneticSolver
  ( solveKnapsackOptimized,
    Solution (..),
  )
where

import KnapsackBruteSolver (getBestSolution)
import System.Random ( Random(randomR), StdGen )
import Data.Foldable (foldl')
import KnapsackData
  ( Cost,
    Item (cost, weight),
    Knapsack (items, maxWeight, minCost),
    ItemsCombination,
    Solution (solCost),
  )

-- Numer of initial population
initialLength :: Int
initialLength = 500
generationsCount :: Int
generationsCount = 20

-- Probabilities of generic events (crossover, mutation)
crossoverProbability :: Double
crossoverProbability = 0.5
mutationProbability :: Double
mutationProbability = 0.2

-- Solve knapsack problem with generic algorithm
solveKnapsackOptimized :: Knapsack -> StdGen -> Maybe Solution
solveKnapsackOptimized knapsack gen =
  let
    (initialGeneration, newGen) = generateInitialGeneration initialLength (length $ items knapsack) gen
    (finalGeneration, _) = getLastGeneration generationsCount initialGeneration newGen where 
      getLastGeneration :: Int -> [ItemsCombination] -> StdGen -> ([ItemsCombination], StdGen)
      getLastGeneration 0 oldGeneration gen' = (oldGeneration, gen')
      getLastGeneration n oldGeneration gen' = 
        let
          (newGeneration, newGen') = generateNewGeneration knapsack oldGeneration gen'
        in
          getLastGeneration (n-1) newGeneration newGen'
    -- From last generation pick best solution with bruteforce
    bestSolution = getBestSolution knapsack finalGeneration
  in
    if solCost bestSolution < minCost knapsack then Nothing else Just bestSolution

-- Creates new generation from old generation with selection crossover and mutation
generateNewGeneration :: Knapsack -> [ItemsCombination] -> StdGen -> ([ItemsCombination], StdGen)
generateNewGeneration knapsack oldGeneration gen =
  let
    (parents, returnGen) = getParents initialLength gen where
      getParents:: Int -> StdGen  -> ([ItemsCombination], StdGen)
      getParents n parentGenerator
        | n <= 0 = ([], parentGenerator)
        | otherwise =
          let
            (newParents, newGen) = generateNewParents knapsack oldGeneration parentGenerator
            (restOfParents, finalGen) = getParents (n - length newParents) newGen
          in
            (newParents ++ restOfParents, finalGen)
    in
    (parents, returnGen)

-- Creates 2 parents based on previous generation
generateNewParents :: Knapsack -> [ItemsCombination] -> StdGen -> ([ItemsCombination], StdGen)
generateNewParents knapsack oldGeneration gen =
  let
    (selected, newGen) = selection knapsack oldGeneration gen
    (crossovered, newGen') = crossover selected newGen
    (firstMutated, newGen'') = mutation (head crossovered) newGen'
    (secondMutated, finalGen) = mutation (last crossovered) newGen''
  in
    ([firstMutated, secondMutated], finalGen)

-- Switches first half of both parents and than second half
crossover :: [ItemsCombination] -> StdGen -> ([ItemsCombination], StdGen)
crossover parents gen =
  let
    (shouldCrossover, newGen) = randomR (0.0, 1.0) gen
    n = length $ head parents
    firstParent = head parents
    secondParent = last parents
    splitPoint = n `div` 2
    firstSplit = splitAt splitPoint firstParent
    secondSplit = splitAt splitPoint secondParent
  in
    if shouldCrossover < crossoverProbability
      then ([fst firstSplit ++ snd secondSplit, fst secondSplit ++ snd firstSplit], newGen)
      else (parents, newGen)

-- With shouldMutate propability changes solution bit
mutation :: ItemsCombination -> StdGen -> (ItemsCombination, StdGen)
mutation [] gen = ([], gen)
mutation (x:xs) gen =
    let
        (shouldMutate, newGen) = randomR (0.0, 1.0) gen
        value = if shouldMutate < mutationProbability
          then if x == 1 then 0 else 1
          else x
        (restOfList, finalGen) = mutation xs newGen
    in  (value:restOfList, finalGen)

-- Picks 4 random combinations and duel them to final 2 best
selection :: Knapsack -> [ItemsCombination] -> StdGen -> ([ItemsCombination], StdGen)
selection knapsack generation gen =
  let (a, resta, newGen) = pickOneRandom generation gen
      (b, restb, newGen') = pickOneRandom resta newGen
      (c, restc, newGen'') = pickOneRandom restb newGen'
      (d, _, finalGen) = pickOneRandom restc newGen''
      firstParent = if getCombinationFitness knapsack a > getCombinationFitness knapsack b then a else b
      secondParent = if getCombinationFitness knapsack c > getCombinationFitness knapsack d then c else d
  in ([firstParent,secondParent], finalGen)

-- Generates random sequence of items with length passed in first argument
randomItemsCombination :: Int -> StdGen -> (ItemsCombination, StdGen)
randomItemsCombination 0 gen = ([], gen)
randomItemsCombination n gen =
    let (value, newGen) = randomR (0,1) gen
        (restOfList, finalGen) = randomItemsCombination (n-1) newGen
    in  (value:restOfList, finalGen)

-- Generates first generation of generic algorithm (totally random)
generateInitialGeneration :: Int -> Int -> StdGen -> ([ItemsCombination], StdGen)
generateInitialGeneration 0 _ gen = ([], gen)
generateInitialGeneration n itemsCount gen =
    let (value, newGen) = randomItemsCombination itemsCount gen
        (restOfList, finalGen) = generateInitialGeneration (n-1) itemsCount newGen
    in  (value:restOfList, finalGen)

-- Returns fitness coeficient of knapsack combination
getCombinationFitness :: Knapsack -> ItemsCombination -> Cost
getCombinationFitness knapsack combination =
  let (combinationWeight, combinationCost) = foldl' (\acc (item, isIncluded) -> (isIncluded * weight item + fst acc, isIncluded * cost item + snd acc)) (0, 0) (zip (items knapsack) combination)
  in if combinationWeight > maxWeight knapsack then 0 else combinationCost

-- -- Pick two elements from array and return them and the rest of the array
pickOneRandom :: [a] -> StdGen -> (a, [a], StdGen)
pickOneRandom xs gen =
  let (idx, newGen) = randomR (0, length xs - 1) gen
      (before, after) = splitAt idx xs
  in (head after, before ++ tail after, newGen)
