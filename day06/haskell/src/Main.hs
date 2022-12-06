module Main (main) where

import Data.List (transpose, nub)

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (show $ findFirstMarker 4 input)
  putStrLn $ "Second answer: " ++ (show $ findFirstMarker 14 input)
  where
    findFirstMarker len = (+ len) . head . positionsBy (isMarker len) . windows len

    windows n xs = transpose [drop n' xs | n' <- [0..(n-1)]]
    positionsBy p = map fst . filter (p . snd) . zip [0..]
    isMarker len = (== len) . length . nub

