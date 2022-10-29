{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.List (sort)
import InputParser
import Util (gaussianSum)

numberListParser :: Parser [Int]
numberListParser = decimal `sepBy` ","

part1 :: [Int] -> Int
part1 xs =
  let median = sort xs !! (length xs `div` 2)
   in sum [abs $ x - median | x <- xs]

part2 :: [Int] -> Int
part2 xs =
  let low = minimum xs
      high = maximum xs
   in minimum [sum [gaussianSum $ abs $ x - y | x <- xs] | y <- [low .. high]]

main :: IO ()
main = do
  test1
  test2

  input <- parseInput 7 numberListParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 7 numberListParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 7 1 numberListParser
  print $ part1 input
  guard $ part1 input == 37
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 7 1 numberListParser
  print $ part2 input
  guard $ part2 input == 168
  print ("ok" :: String)
