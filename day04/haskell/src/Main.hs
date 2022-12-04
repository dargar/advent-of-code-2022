module Main (main) where

import Data.Char (isDigit)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (show $ firstAnswer input)
  putStrLn $ "Second answer: " ++ (show $ secondAnswer input)
  where
    firstAnswer = length . filter fullyOverlaps . chunksOf 4 . parseNumbers
    secondAnswer = length . filter partlyOverlaps . chunksOf 4 . parseNumbers

    parseNumbers = map read . words . map (replaceNonDigit ' ')
    replaceNonDigit r c = if isDigit c then c else r

    fullyOverlaps :: [Int] -> Bool
    fullyOverlaps [a0, a1, b0, b1] = a0 <= b0 && b1 <= a1 || b0 <= a0 && a1 <= b1
    fullyOverlaps _ = undefined

    partlyOverlaps :: [Int] -> Bool
    partlyOverlaps [a0, a1, b0, b1] = a0 <= b0 && b0 <= a1 || b0 <= a0 && a0 <= b1
    partlyOverlaps _ = undefined
