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
      let crates = parseCrates $ head sections in
      let instructions = parseInstructions $ head $ tail sections in
      let movedCrates = foldl movingFunction crates instructions in
      topCrates movedCrates

    parseCrates = filter (not . null) . map (filter isAlpha) . transpose . lines
    parseInstructions = map (mapMaybe readMaybe) . map words . lines

    moveCratesOneByOne crates instruction = moveCrates reverse crates instruction
    moveCratesAsStack crates instruction = moveCrates id crates instruction

    moveCrates f crates [n, from, to] =
      let (cs, crates') = takeFrom (from - 1) n crates in
      appendTo (to - 1) (f cs) crates'
    moveCrates _ _ _ = undefined

    takeFrom i n xss =
      let (removed, remaining) = splitAt n $ xss !! i in
      (removed, take i xss ++ [remaining] ++ drop (i + 1) xss)
    appendTo i xs xss =
      let updated = xs ++ xss !! i in
      take i xss ++ [updated] ++ drop (i + 1) xss

    topCrates = map head . filter (not . null)

