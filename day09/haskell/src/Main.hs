module Main (main) where

import Control.Arrow
import qualified Data.Set as S

main :: IO ()
main = interact $ parseWith [2, 10] >>> solve >>> present

parseWith :: [Int] -> String -> [([(Int, Int)], [String])]
parseWith = (uncurry zip . (knots *** motions)) .: (,)
  where
    knots = map ((flip replicate) (0, 0))

    motions = repeat . expandMotions . map (readMotion . words) . lines
    readMotion [dir, steps] = (dir, read steps)
    readMotion _ = undefined
    expandMotions = concatMap (uncurry $ flip replicate)

solve :: [([(Int, Int)], [String])] -> [Int]
solve = map (S.size . S.fromList . map last . uncurry (scanl stepKnots))

stepKnots :: [(Int, Int)] -> String -> [(Int, Int)]
stepKnots = scanl1 stepTail .: stepHead

stepHead :: [(Int, Int)] -> String -> [(Int, Int)]
stepHead ((hx, hy):ts) "U" = (hx, hy + 1) : ts
stepHead ((hx, hy):ts) "D" = (hx, hy - 1) : ts
stepHead ((hx, hy):ts) "L" = (hx - 1, hy) : ts
stepHead ((hx, hy):ts) "R" = (hx + 1, hy) : ts
stepHead _ _ = undefined

stepTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
stepTail (hx, hy) (tx, ty)
  | abs (hx - tx) > 1 || abs (hy - ty) > 1 =
    let tx' = if hx < tx then tx - 1 else if hx > tx then tx + 1 else tx in
    let ty' = if hy < ty then ty - 1 else if hy > ty then ty + 1 else ty in
    (tx', ty')
  | otherwise = (tx, ty)

present :: [Int] -> String
present = unlines . map format . zip [1..]
  where
    format (part, answer) = "Part " ++ show (part :: Int) ++ ": " ++ show (answer :: Int)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
