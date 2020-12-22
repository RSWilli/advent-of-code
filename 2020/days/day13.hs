{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Char (intToDigit)
import InputParser
import Util

type Schedule = (Int, [Maybe Int])

listParser :: Parser [Maybe Int]
listParser = ((Nothing <$ "x") <|> (Just <$> decimal)) `sepBy` ","

scheduleParser :: Parser Schedule
scheduleParser = (,) <$> (decimal <* newline) <*> listParser

part1 :: Schedule -> Int
part1 (now, busses) = uncurry (*) $ minimum [(mod (- now) x, x) | Just x <- busses]

part2 :: Schedule -> Int
part2 (_, busses) = fst $ foldl1 subsequent [(mod (- i) b, b) | (i, Just b) <- zip [0 ..] busses]
  where
    subsequent (offset, bus1) (diff, bus2)
      | offset `mod` bus2 == diff = (offset, bus1 * bus2)
      | otherwise = subsequent (offset + bus1, bus1) (diff, bus2)

main = do
  sched <- parseInput 13 scheduleParser
  print $ part1 sched
  test1
  test2
  test3
  print $ part2 sched
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 13 scheduleParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 sched,
          bench "part2" $ whnf part2 sched
        ]
    ]

test1 = do
  sched <- parseTest 13 1 scheduleParser
  guard $ part1 sched == 295
  print "ok"

test2 = do
  sched <- parseTest 13 1 scheduleParser
  guard $ part2 sched == 1068781
  print "ok"

test3 = do
  sched <- parseTest 13 2 scheduleParser
  guard $ part2 sched == 3417
  print "ok"