{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import InputParser
import Util

numberparser :: Parser [Int]
numberparser = decimal `sepBy` ","

gamesteps :: IntMap Int -> Int -> Int -> Int -> Int
gamesteps spoken lastNum turn index =
  let speek = turn - 1 - M.findWithDefault (turn - 1) lastNum spoken
      spoken' = M.insert lastNum (turn -1) spoken
   in if index == 0 then speek else gamesteps spoken' speek (turn + 1) (index - 1)

part1 nums =
  let spoken = M.fromList $ zip (init nums) [0 ..]
      lastNum = last nums
      turn = length nums
   in gamesteps spoken lastNum turn (2020 - 1 - turn)

part2 nums =
  let spoken = M.fromList $ zip (init nums) [0 ..]
      lastNum = last nums
      turn = length nums
   in gamesteps spoken lastNum turn (30000000 - 1 - turn)

main = do
  nums <- parseInput 15 numberparser
  print $ part1 nums
  print $ part2 nums
  -- test1
  -- test2

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 15 numberparser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 nums,
          bench "part2" $ whnf part2 nums
        ]
    ]

test1 = do
  nums <- parseTest 15 1 numberparser
  guard $ part1 nums == 436
  print "ok"

test2 = do
  nums <- parseTest 15 1 numberparser
  guard $ part2 nums == 175594
  print "ok"
