module Main (main) where

import Control.Applicative ((<|>))
import Control.Arrow
import Text.ParserCombinators.ReadP
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Monkey = Monkey
            { mnkIdentity :: Int
            , mnkItems :: [Int]
            , mnkOperation :: Int -> Int
            , mnkDivisor :: Int
            , mnkTest :: Int -> Int -> Int
            }

main :: IO ()
main = interact $ present . (part1 &&& part2) . parse
  where
    part1 = solve 20 (flip div 3)
    part2 = uncurry (solve 10000) . (flip mod . product . map mnkDivisor &&& id)

parse :: String -> [Monkey]
parse = fst . head . readP_to_S readMonkeys . trim

readMonkeys :: ReadP [Monkey]
readMonkeys = do
  monkeys <- readMonkey `sepBy` count 2 (char ('\n'))
  optional (char '\n')
  eof
  return monkeys
  where
    eol = char '\n' >> return () <|> eof
    number = munch1 C.isDigit
    word = munch1 (not . C.isSpace)

    readMonkey = do
      identity <- readIdentity
      startingItems <- readStartingItems
      operation <- readOperation
      divisor <- readDivisor
      test <- readTest
      return Monkey { mnkIdentity = identity
                    , mnkItems = startingItems
                    , mnkOperation = operation
                    , mnkDivisor = divisor
                    , mnkTest = test
                    }
    readIdentity = fmap read $ string "Monkey" *> skipSpaces *> number <* char ':' <* eol
    readStartingItems = fmap (map read) $ skipSpaces *> string "Starting items: " *> number `sepBy` string ", " <* eol
    readOperation = do
      skipSpaces >> string "Operation:" >> skipSpaces >> string "new" >> skipSpaces >> char '=' >> return ()
      lhs <- skipSpaces >> word
      op <- skipSpaces >> char '+' <|> char '*'
      rhs <- skipSpaces >> word
      return $ case (lhs, op, rhs) of
        ("old", '+', "old") -> \old -> old + old
        ("old", '+', n) -> \old -> old + (read n)
        ("old", '*', "old") -> \old -> old * old
        ("old", '*', n) -> \old -> old * (read n)
        _ -> undefined
    readDivisor = fmap read $ skipSpaces >> string "Test: divisible by" >> skipSpaces >> number
    readTest = do
      ifTrue <- skipSpaces >> string "If true: throw to monkey" >> skipSpaces >> number
      ifFalse <- skipSpaces >> string "If false: throw to monkey" >> skipSpaces >> number
      return $ \d n -> if n `mod` d == 0 then read ifTrue else read ifFalse

trim :: String -> String
trim = reverse . dropWhile C.isSpace . reverse

solve :: Int -> (Int -> Int) -> [Monkey] -> Int
solve rounds relief monkeys =
  product
  $ take 2
  $ L.reverse
  $ L.sort
  $ map (snd . snd)
  $ M.toList
  $ L.foldl' (nextState relief) (startingState monkeys)
  $ (take (rounds * length monkeys) $ cycle monkeys)

startingState :: [Monkey] -> M.Map Int ([Int], Int)
startingState = M.fromList . map (\(Monkey identity items _ _ _) -> (identity, (items, 0)))

nextState :: (Int -> Int) -> M.Map Int ([Int], Int) -> Monkey -> M.Map Int ([Int], Int)
nextState relief is (Monkey i _ op divisor test) =
  let (items, thrownCount) = is M.! i in
  let thrownItems = map (test divisor &&& id) $ map relief $ map op items in
  let nextItems = M.insert i ([], thrownCount + length items) is in
  let nextItems' = L.foldl' (\ms (j, v) -> M.adjust (\(xs, c) -> (v:xs, c)) j ms) nextItems thrownItems in
  nextItems'

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines ["First answer: " ++ show a, "Second answer: " ++ show b]

