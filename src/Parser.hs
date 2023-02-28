-- Module for parsing string to Knapsack object

module Parser
(   parseToKnapsack,
)   where

import Text.ParserCombinators.Parsec
import System.Environment
import Knapsack

-- Parse information about items in knapsack
parseItem :: GenParser Char st Item
parseItem = do
    spaces
    string "Item {"
    spaces
    string "weight:"
    spaces
    weight <- many1 digit
    spaces
    string "cost:"
    spaces
    cost <- many1 digit
    spaces
    char '}'
    spaces
    return $ Item (read weight) (read cost)
    
-- Parse information about knapsack max weight and min cost
parseKnapsack :: GenParser Char st Knapsack
parseKnapsack = do
    spaces
    string "Knapsack {"
    spaces
    string "maxWeight:"
    spaces
    maxWeight <- many1 digit
    spaces
    string "minCost:"
    spaces
    minCost <- many1 digit
    spaces
    string "items:"
    spaces
    char '['
    spaces
    items <- sepBy parseItem spaces
    spaces
    char ']'
    spaces
    char '}'
    spaces
    return $ Knapsack (read maxWeight) (read minCost) items

-- Parse string to Knapsack object
parseToKnapsack :: String -> Knapsack
parseToKnapsack "" = error "Empty input file."
parseToKnapsack file = case parse parseKnapsack "" file of
                Left e -> error (show e)
                Right knapsack -> knapsack