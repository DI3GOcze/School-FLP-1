-- Funkcionální projekt do předmětu FLP
-- Autor: Jakub Kryštůfek (xkryst02)
-- Rok: 2023

import System.Environment
import System.Random
import KnapsackParser (parseToKnapsack)
import KnapsackBruteSolver (solveKnapsackBruteforce)
import KnapsackGeneticSolver (solveKnapsackOptimized)

printHelp :: IO ()
printHelp = putStrLn "Usage: flp22-fun OPTION \n\nPossible run options: \n -i filepath \t Info about knapsack \n -b filepath \t Solve knapsack with brute force \n -o filepath \t Solve knapsack with optimized algorithm \n"

switchFunctionality :: String -> String -> StdGen -> IO ()
switchFunctionality switcher input gen = do
  case switcher of
    "-i" -> print (parseToKnapsack input)
    "-b" -> case solveKnapsackBruteforce (parseToKnapsack input) of
      Nothing -> print False
      Just solution -> print solution
    "-o" -> case solveKnapsackOptimized (parseToKnapsack input) gen of
      Nothing -> print False
      Just solution -> print solution
    _ -> printHelp

main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen  
  case args of
    [switcher, filename] -> do
      input <- readFile filename 
      switchFunctionality switcher input gen
    [switcher] -> do
      input <- getContents
      switchFunctionality switcher input gen
    _ -> printHelp
