module Main (main) where

import qualified Data.List as L
import qualified Data.Maybe as MB

data Entry = File [String] Int
           | Directory [String]
  deriving (Eq)

main :: IO ()
main = interact $ unlines . present . solve . lines

present :: [Int] -> [String]
present = map (\(part, answer) -> "Part " ++ show part ++ ": " ++ show answer) . zip [1..]

solve :: [String] -> [Int]
solve ls = let dirSizes = map du $ filter isDir entries in
           let totalSize = 70000000 in
           let requiredSize = 30000000 in
           let rootSize = maximum dirSizes in
           let availableSize = totalSize - rootSize in
           let sizeToFree = requiredSize - availableSize in
           [ sum $ filter (<= 100000) dirSizes
           , minimum $ filter (>= sizeToFree) dirSizes ]
  where
    apply (path, es) ["$", "cd", ".."] = (tail path, es)
    apply (path, es) ["$", "cd", d] = (d:path, es)
    apply (path, es) ["$", "ls"] = (path, es)
    apply (path, es) ["dir", d] = (path, (Directory (d:path)):es)
    apply (path, es) [size, f] = (path, (File (f:path) (read size)):es)
    apply _ _ = undefined

    entries = snd $ L.foldl' apply ([], [(Directory ["/"])]) $ map words ls

    du entry = MB.fromMaybe 0 $ L.lookup entry [(e, du' e) | e <- entries]

    du' (File _ size) = size
    du' directory = sum $ map du $ filter (isChild directory) entries

    isChild parent child = (getPath parent) == (tail $ getPath child)

    isDir (Directory _) = True
    isDir (File _ _) = False

    getPath (File path _) = path
    getPath (Directory path) = path
