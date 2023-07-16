module Main (main) where

import Control.Arrow
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = interact $ parse >>> (solve ['S'] &&& solve ['S', 'a']) >>> present

parse :: String -> Map (Int, Int) Char
parse s = M.fromList [((x, y), c) | (y,cs) <- zip [0..] (lines s),
                                    (x, c) <- zip [0..] cs]

solve :: [Char] -> Map (Int, Int) Char -> Int
solve startingHeights heightMap = length
                                $ takeWhile (S.disjoint targetPoints . snd)
                                $ iterate step (S.empty, startingPoints)
  where
    startingPoints = M.keysSet $ M.filter (`elem` startingHeights) heightMap
    targetPoints = M.keysSet $ M.filter (== 'E') heightMap

    step (visited, frontier) =
      let
        visited' = visited `S.union` frontier
        frontier' = (`S.difference` visited)
                  $ S.unions
                  $ S.map nextSteps frontier
      in (visited', frontier')

    nextSteps = S.fromList
              . map snd
              . filter (uncurry (stepable `on` ((M.!) heightMap)))
              . filter ((`M.member` heightMap) . snd)
              . neighbours
    neighbours p = [(p, f g p) | f <- [first, second], g <- [succ, pred]]
    stepable = (<= 1) .: (flip (-)) `on` fromEnum . normalizeHeight
    normalizeHeight 'S' = 'a'
    normalizeHeight 'E' = 'z'
    normalizeHeight h = h

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b
                         ]

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
