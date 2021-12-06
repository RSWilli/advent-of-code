{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import InputParser

type Fishes = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

lanternfishParser :: Parser Fishes
lanternfishParser = toTuple <$> (decimal `sepBy` ",")
  where
    toTuple :: [Int] -> Fishes
    toTuple = foldl countNumbers (0, 0, 0, 0, 0, 0, 0, 0, 0)
    countNumbers (a, b, c, d, e, f, g, h, i) n =
      case n of
        0 -> (a + 1, b, c, d, e, f, g, h, i)
        1 -> (a, b + 1, c, d, e, f, g, h, i)
        2 -> (a, b, c + 1, d, e, f, g, h, i)
        3 -> (a, b, c, d + 1, e, f, g, h, i)
        4 -> (a, b, c, d, e + 1, f, g, h, i)
        5 -> (a, b, c, d, e, f + 1, g, h, i)
        6 -> (a, b, c, d, e, f, g + 1, h, i)
        7 -> (a, b, c, d, e, f, g, h + 1, i)
        8 -> (a, b, c, d, e, f, g, h, i + 1)
        _ -> error "Invalid number"

simulateDay :: Fishes -> Fishes
simulateDay (f0, f1, f2, f3, f4, f5, f6, f7, f8) = (f0', f1', f2', f3', f4', f5', f6', f7', f8')
  where
    f0' = f1
    f1' = f2
    f2' = f3
    f3' = f4
    f4' = f5
    f5' = f6
    f6' = f7 + f0
    f7' = f8
    f8' = f0

simulate :: Int -> Fishes -> Fishes
simulate c = foldr (.) id (replicate c simulateDay)

countFishes :: Fishes -> Int
countFishes (f0, f1, f2, f3, f4, f5, f6, f7, f8) = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8

part1 :: Fishes -> Int
part1 = countFishes . simulate 80

part2 :: Fishes -> Int
part2 = countFishes . simulate 256

main :: IO ()
main = do
  test1

  test2

  input <- parseInput 6 lanternfishParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 6 lanternfishParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 6 1 lanternfishParser
  guard $ part1 input == 5934
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 6 1 lanternfishParser
  guard $ part2 input == 26984457539
  print ("ok" :: String)
