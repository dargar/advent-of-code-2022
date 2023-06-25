module Main (main) where

import Control.Arrow
import qualified Data.Char as C
import qualified Data.List as L

main :: IO ()
main = interact $ fromString >>> part1 &&& part2 >>> present

fromString :: String -> [[Int]]
fromString = (map . map) C.digitToInt . lines

part1 :: [[Int]] -> Int
part1 = length
      . filter id
      . concat 
      . sweep visibleFromOutside (||)

part2 :: [[Int]] -> Int
part2 = maximum
      . concat
      . sweep viewingDistance (*)

sweep :: ([a] -> [b]) -> (b -> b -> b) -> [[a]] -> [[b]]
sweep f g = unrotations g . (map . map) f . rotations
  where
    rotations = take 4 . iterate rotateRight
    unrotations merge = L.foldl1' ((zipWith . zipWith) merge)
                      . map (uncurry (times rotateLeft)) . zip [0..]

visibleFromOutside :: [Int] -> [Bool]
visibleFromOutside = reverse . snd . L.foldl' (\(h, xs) x -> (max x h, (x > h):xs)) (-1, [])

viewingDistance :: [Int] -> [Int]
viewingDistance = map view . sublists
  where
    view [] = 0
    view (y:ys) = length $ takeUntil (< y) ys

rotateRight :: [[a]] -> [[a]]
rotateRight = map reverse . L.transpose

rotateLeft :: [[a]] -> [[a]]
rotateLeft = L.transpose . map reverse

times :: (a -> a) -> Int -> a -> a
times f n = (flip (!!)) n . iterate f

sublists :: [a] -> [[a]]
sublists = takeWhile (not . null) . iterate (drop 1)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = if p x then x : takeUntil p xs else [x]

present :: (Show a, Show b) => (a, b) -> String
present = answer 1 *** answer 2 >>> asList >>> unlines
  where
    answer n a = "Part " ++ show (n :: Int) ++ ": " ++ show a
    asList (a, b) = [a, b]
