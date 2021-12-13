{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.List (permutations, sort)
import qualified Data.Map as M
import InputParser
import Util (lIntListToInt)

sevenSegParser :: Parser ([String], [String])
sevenSegParser = (,) <$> (map sort <$> many text) <* "| " <*> (map sort <$> many text)

--  aaaa
-- b    c
-- b    c
--  dddd
-- e    f
-- e    f
--  gggg

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....

--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg

-- number of occurences of each segment in the digits
a, b, c, d, e, f, g :: Int
a = 8
b = 6
c = 8
d = 7
e = 4
f = 9
g = 7

-- a map that the sum of the number of occurences of each segment in the digits to the digit
digitScores :: M.Map Int Int
digitScores =
  M.fromList
    [ (a + b + c + e + f + g, 0),
      (c + f, 1),
      (a + c + d + e + g, 2),
      (a + c + d + f + g, 3),
      (b + c + d + f, 4),
      (a + b + d + f + g, 5),
      (a + b + d + e + f + g, 6),
      (a + c + f, 7),
      (a + b + c + d + e + f + g, 8),
      (a + b + c + d + f + g, 9)
    ]

getValidMapping :: ([String], [String]) -> [Int]
getValidMapping (inp, out) = do
  -- count the occurences of each segment in the digits
  let counts = foldr (\x -> M.insertWith (+) x 1) M.empty $ concat inp
  num <- out
  -- add the scores digits of the segments in the output together, and map them to the digit
  let score = sum $ map (counts M.!) num
  return $ digitScores M.! score

part1 :: [([String], [String])] -> Int
part1 = length . filter (\x -> x == 2 || x == 3 || x == 4 || x == 7) . concatMap (map length . snd)

part2 :: [([String], [String])] -> Int
part2 = sum . map (lIntListToInt 10 . getValidMapping)

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
