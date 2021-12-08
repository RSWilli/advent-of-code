{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.List (permutations, sort)
import qualified Data.Map as M
import InputParser

sevenSegParser :: Parser ([String], [String])
sevenSegParser = (,) <$> (map sort <$> many text) <* "| " <*> (map sort <$> many text)

--  aaaa
-- b    c
-- b    c
--  dddd
-- e    f
-- e    f
--  gggg

segments :: String
segments = "abcdefg"

sevenSegmentToInt :: M.Map String Int
sevenSegmentToInt = M.fromList [("abcefg", 0), ("cf", 1), ("acdeg", 2), ("acdfg", 3), ("bcdf", 4), ("abdfg", 5), ("abdefg", 6), ("acf", 7), ("abcdefg", 8), ("abcdfg", 9)]

decode :: String -> Int
decode x = sevenSegmentToInt M.! x

allPossibleMappings :: [M.Map Char Char]
allPossibleMappings = map (M.fromList . zip segments) $ permutations segments

toInt :: [Int] -> Int
toInt = foldl (\acc x -> acc * 10 + x) 0

getValidMapping :: ([String], [String]) -> [Int]
getValidMapping (inp, out) = do
  mapping <- allPossibleMappings
  let translate xs = map (sort . map (mapping M.!)) xs
  let mapped = translate inp
  guard $ all (`M.member` sevenSegmentToInt) mapped
  map decode $ translate out

part1 :: [([String], [String])] -> Int
part1 xs = length $ filter (\x -> x == 2 || x == 3 || x == 4 || x == 7) $ concatMap (map length . snd) xs

part2 :: [([String], [String])] -> Int
part2 xs = sum $ map (toInt . getValidMapping) xs

main :: IO ()
main = do
  test1
  test2

  input <- parseInputLines 8 sevenSegParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 8 sevenSegParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTestLines 8 1 sevenSegParser
  guard $ part1 input == 26
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTestLines 8 1 sevenSegParser
  guard $ part2 input == 61229
  print ("ok" :: String)
