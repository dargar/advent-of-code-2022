module Main (main) where

import Data.List (transpose, nub)

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (show $ findFirstMarker 4 input)
  putStrLn $ "First answer: " ++ (show $ findFirstMarker 14 input)
  where
    findFirstMarker seqLen = fst . head . filter (isMarker . snd) . zip [seqLen..] . windows seqLen

    windows n xs = transpose [drop n' xs | n' <- [0..(n-1)]]
    isMarker signalSequence =
      let originalLength = length signalSequence in
      let dedupedLength = length $ nub signalSequence in
      originalLength == dedupedLength

