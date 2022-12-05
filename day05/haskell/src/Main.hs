module Main (main) where

import Data.List.Split (splitOn)
import Data.Char (isAlpha)
import Data.List (transpose)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (answer moveCratesOneByOne input)
  putStrLn $ "Second answer: " ++ (answer moveCratesAsStack input)
  where
    answer movingFunction input =
      let sections = splitOn "\n\n" input in
      let initialCrates = parseCrates $ sections !! 0 in
      let instructions = parseInstructions $ sections !! 1 in
      let movedCrates = foldl movingFunction initialCrates instructions in
      topCrates movedCrates

    parseCrates = map (filter isAlpha) . map snd . filter ((== 0) . flip mod 4 . fst) . zip [0..] . drop 1 . transpose . lines
    parseInstructions = map (mapMaybe readMaybe) . map words . lines

    moveCratesOneByOne crates [n, from, to] = head $ drop n $ iterate (moveCratesOneByOne' from to) crates
    moveCratesOneByOne crates _ = undefined

    moveCratesOneByOne' from to crates =
      let (as, b:bs) = splitAt (from - 1) crates in
      let crate = head b in
      let crates' = as ++ (drop 1 b):bs in
      let (as, b:bs) = splitAt (to - 1) crates' in
      as ++ (crate:b):bs

    moveCratesAsStack crates [n, from, to] =
      let (as, b:bs) = splitAt (from - 1) crates in
      let cs = take n b in
      let crates' = as ++ (drop n b):bs in
      let (as, b:bs) = splitAt (to - 1) crates' in
      as ++ (cs ++ b):bs
    moveCratesAsStack crates _ = undefined

    topCrates = map head . filter (not . null)
