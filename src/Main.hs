import System.Environment
import KnapsackSolver
import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        [switcher, filename] -> do
            input <- readFile filename
            case switcher of
                "-i" -> print (parseToKnapsack input)
                "-b" -> case solveKnapsackBruteforce (parseToKnapsack input) of
                    (False, _) -> print False
                    (True, solution) -> print solution
                "-o" -> print "Jdu optimalizovat"
                _   -> print "Spatne pouziti programu"

        _ -> putStrLn "Usage: program_name filename"