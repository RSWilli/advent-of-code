import Bench
import Control.Monad (guard)
import Data.List (zip3)
import InputParser

countIncreasing xs window = length $ filter (\(a, b) -> a < b) $ zip xs $ drop window xs

part1 xs = countIncreasing xs 1

part2 xs = countIncreasing xs 3

main = do
  test1
  test2

  input <- parseInputLines 1 number
  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 1 number)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 = do
  list <- parseTestLines 1 1 number
  guard $ part1 list == 7
  print "ok"

test2 = do
  list <- parseTestLines 1 1 number
  guard $ part2 list == 5
  print "ok"
