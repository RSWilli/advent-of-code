{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import InputParser
import Util

seatParser :: Parser Int
seatParser = foldl (\sum i -> sum * 2 + i) 0 <$> many (1 <$ ("B" <|> "R") <|> (0 <$ ("F" <|> "L")))

bounds :: Ord a => [a] -> (a, a)
bounds (x : xs) = foldl (\(min', max') num -> (min min' num, max max' num)) (x, x) xs

sumTo :: Int -> Int
sumTo x = x * (x + 1) `div` 2

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 passes =
  let (min, max) = bounds passes
      lowerSum = sumTo $ min - 1
      fullSum = sumTo max
      actualSum = sum passes
   in (fullSum - lowerSum) - actualSum

main = do
  passes <- parseInputLines 5 seatParser
  print $ part1 passes
  print $ part2 passes
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 5 seatParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 passes,
          bench "part2" $ whnf part2 passes
        ]
    ]