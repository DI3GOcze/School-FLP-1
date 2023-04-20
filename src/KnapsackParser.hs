-- Funkcionální projekt do předmětu FLP
-- Autor: Jakub Kryštůfek (xkryst02)
-- Rok: 2023

module KnapsackParser
  ( parseToKnapsack,
  )
where

import KnapsackData
import Text.ParserCombinators.Parsec

-- Parse information about items in knapsack
parseItem :: GenParser Char st Item
parseItem = do
  spaces
  _ <- string "Item {"
  spaces
  _ <- string "weight:"
  spaces
  weight' <- many1 digit
  spaces
  _ <- string "cost:"
  spaces
  cost' <- many1 digit
  spaces
  _ <- char '}'
  spaces
  return $ Item (read weight') (read cost')

-- Parse information about knapsack max weight and min cost
parseKnapsack :: GenParser Char st Knapsack
parseKnapsack = do
  spaces
  _ <- string "Knapsack {"
  spaces
  _ <- string "maxWeight:"
  spaces
  maxWeight' <- many1 digit
  spaces
  _ <- string "minCost:"
  spaces
  minCost' <- many1 digit
  spaces
  _ <- string "items:"
  spaces
  _ <- char '['
  spaces
  items' <- sepBy parseItem spaces
  spaces
  _ <- char ']'
  spaces
  _ <- char '}'
  spaces
  return $ Knapsack (read maxWeight') (read minCost') items'

-- Parse string to Knapsack object
parseToKnapsack :: String -> Knapsack
parseToKnapsack "" = error "Empty input file."
parseToKnapsack file = case parse parseKnapsack "" file of
  Left e -> error (show e)
  Right knapsack -> knapsack