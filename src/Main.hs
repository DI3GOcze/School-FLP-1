import KnapsackBruteSolver
import Parser
import System.Environment

printHelp :: IO ()
printHelp = putStrLn "Usage: flp22-fun OPTION \n\nPossible run options: \n -i filepath \t Info about knapsack \n -b filepath \t Solve knapsack with brute force \n -o filepath \t Solve knapsack with optimized algorithm \n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [switcher, filename] -> do
      input <- readFile filename
      case switcher of
        "-i" -> print (parseToKnapsack input)
        "-b" -> case solveKnapsackBruteforce (parseToKnapsack input) of
          Nothing -> print False
          Just solution -> print solution
        "-o" -> print "Jdu optimalizovat"
        _ -> printHelp
    _ -> printHelp
