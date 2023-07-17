module Main (main) where

import Control.Applicative
import Control.Arrow
import Text.ParserCombinators.ReadP
import qualified Data.Char as C
import qualified Data.List as L

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

data Packet = PacketL [Packet]
            | PacketI Int
  deriving (Show, Eq)

instance Ord Packet where
  compare (PacketI l) (PacketI r) = compare l r
  compare (PacketL ls) (PacketL rs) =
    case filter (/= EQ) $ map (uncurry compare) $ zip ls rs of
      [] -> compare (length ls) (length rs)
      (o:_) -> o
  compare (PacketI l) rs = compare (PacketL [PacketI l]) rs
  compare ls (PacketI r) = compare ls (PacketL [PacketI r])

parse :: String -> [Packet]
parse = map (fst . head . readP_to_S parsePacket) . filter (not . null) . lines
  where
    parsePacket = parseInteger <|> parseList
    parseInteger = PacketI . read <$> munch1 C.isDigit
    parseList = PacketL <$> (char '[' *> parsePacket `sepBy` char ',' <* char ']')

part1 :: [Packet] -> Int
part1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..] . pairwise
  where
    pairwise [] = []
    pairwise (a:b:xs) = (a, b) : pairwise xs
    pairwise _ = undefined

part2 :: [Packet] -> Int
part2 = product . map fst . filter ((`elem` dividerPackets) . snd) . zip [1..] . L.sort . (++ dividerPackets)
  where
    dividerPackets = [PacketL [PacketL [PacketI 2]], PacketL [PacketL [PacketI 6]]]

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b]
