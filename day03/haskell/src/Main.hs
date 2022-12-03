module Main (main) where

import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (show $ firstAnswer input)
  putStrLn $ "Second answer: " ++ (show $ secondAnswer input)
  where
    firstAnswer  = sum . map priority . map findCommonItem . map halve        . lines
    secondAnswer = sum . map priority . map findCommonItem . Split.chunksOf 3 . lines

    halve xs = [take (length xs `div` 2) xs, drop (length xs `div` 2) xs]
    priority x = if Char.isLower x then 1 + (Char.ord x) - (Char.ord 'a') else 27 + (Char.ord x) - (Char.ord 'A')
    findCommonItem = head . Set.toList . foldl1 Set.intersection . map Set.fromList 
