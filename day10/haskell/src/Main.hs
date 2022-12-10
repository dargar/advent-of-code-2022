module Main (main) where

main :: IO ()
main = do
  input <- readFile "../input"
  let ss = reverse $ states input
  putStrLn $ "First answer: " ++ firstAnswer ss
  putStrLn $ "Second answer:\n" ++ secondAnswer ss
  where
    firstAnswer = show . sum . map (uncurry (*)) . take 6 . everyNth 40 . drop 19 . zip [1..]
    secondAnswer = unlines . chunksOf 40 . map pixel . zip screenCoords

states :: String -> [Int]
states = snd . foldl exec (1, []) . map words . lines
  where
    exec (x, ss) ["noop"]    = (x, x:ss)
    exec (x, ss) ["addx", n] = (x + read n, x:x:ss)
    exec _ _ = undefined

screenCoords :: [Int]
screenCoords = map (flip mod 40) [0..]

pixel :: (Int, Int) -> Char
pixel (screenX, x) = if x - 1 <= screenX && screenX <= x + 1 then '#' else '.'

everyNth :: Int -> [a] -> [a]
everyNth n = map snd . filter ((== 0) . flip mod n . fst) . zip [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

