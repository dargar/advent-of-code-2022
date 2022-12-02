module Main (main) where

main :: IO ()
main = do
  input <- readFile "../input"
  putStrLn $ "First answer: " ++ (show $ firstAnwer input)
  putStrLn $ "Second answer: " ++ (show $ secondAnswer input)
  where
    firstAnwer = sum . map score . lines
    secondAnswer = sum . map (score . translate) . lines
    
    score "A X" = 1 + 3
    score "A Y" = 2 + 6
    score "A Z" = 3 + 0
    score "B X" = 1 + 0
    score "B Y" = 2 + 3
    score "B Z" = 3 + 6
    score "C X" = 1 + 6
    score "C Y" = 2 + 0
    score "C Z" = 3 + 3
    score _ = undefined
    
    translate "A X" = "A Z"
    translate "A Y" = "A X"
    translate "A Z" = "A Y"
    translate "B X" = "B X"
    translate "B Y" = "B Y"
    translate "B Z" = "B Z"
    translate "C X" = "C Y"
    translate "C Y" = "C Z"
    translate "C Z" = "C X"
    translate _ = undefined

