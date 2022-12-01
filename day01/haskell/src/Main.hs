module Main (main) where

import Data.List.Split
import Data.Sort

main :: IO ()
main = do
  input <- readFile "../input"
  let inventories = collectInventories input
  putStrLn $ "First answer: " ++ firstAnswer inventories
  putStrLn $ "Second answer: " ++ secondAnswer inventories
  where
    collectInventories = reverse . sort . map sumInventory . splitOn "\n\n"
    sumInventory = sum . map read . lines
    firstAnswer = show . head
    secondAnswer = show . sum . take 3

