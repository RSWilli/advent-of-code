{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Char (intToDigit)
import Data.Maybe (catMaybes)
import InputParser
import Util

type Schedule = (Int, [Maybe Int])

--https://gist.github.com/trevordixon/6895802
extendedEu :: Integer -> Integer -> (Integer, Integer)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = extendedEu b r

eEuA a b = let (x, y) = extendedEu a b in (mod x a, mod y b)

listParser :: Parser [Maybe Int]
listParser = ((Nothing <$ "x") <|> (Just <$> decimal)) `sepBy` ","

scheduleParser :: Parser Schedule
scheduleParser = (,) <$> (decimal <* newline) <*> listParser

diffTo :: Int -> Int -> Int
diffTo now sched = mod (div now sched * sched - now) sched

part1 :: Schedule -> Int
part1 (now, busses) =
  let withDiffs = map (\x -> (diffTo now x, x)) (catMaybes busses)
      (diff, id) = minimum withDiffs
   in id * diff

part2 :: Schedule -> Int
part2 (now, busses) =
  let withDiffs = catMaybes $ zipWith (\x y -> fmap (x,) y) [0 ..] busses
   in fst $ allSubsequent withDiffs

subsequent bus1 bus2 offset diff =
  let d = (bus2 - diff) `mod` bus2
      arrivals i =
        if (bus1 * i + offset) `mod` bus2 == d
          then (bus1 * i + offset, bus1 * bus2)
          else arrivals (i + 1)
   in arrivals 0

allSubsequent (bus : busses) =
  foldl
    (\(index, b1) (diff, b2) -> subsequent b1 b2 index diff)
    bus
    busses

main = do
  sched <- parseInput 13 scheduleParser
  print $ catMaybes $ zipWith (\x y -> fmap (x,) y) [0 ..] $ snd sched
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