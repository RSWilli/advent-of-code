{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (sort)
import Data.Maybe (mapMaybe)
import InputParser
import Util

count :: (a -> Bool) -> [a] -> Int
count pred xs = length $ filter pred xs

part1 :: [Int] -> Int
part1 adaps =
  let sortedAdaps = sort adaps
      diffs = zipWith (-) sortedAdaps (0 : sortedAdaps)
   in (1 + count (3 ==) diffs) * count (1 ==) diffs

countPaths :: M.IntMap Int -> [Int] -> Int
countPaths m = sum . mapMaybe (m M.!?)

next i = [i + 1, i + 2, i + 3]

part2 :: [Int] -> Int
part2 adaps =
  let possible = S.fromList $ 0 : adaps
      phone = 3 + S.findMax possible
      paths =
        S.foldr
          ( \node paths ->
              let pathsum = countPaths paths (next node)
               in M.insert node pathsum paths
          )
          (M.singleton phone 1)
          possible
   in paths M.! 0

main = do
  adaps <- parseInputLines 10 number
  print $ part1 adaps
  print $ part2 adaps
  test1
  test2
  test3
  test4
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 10 number)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 adaps,
          bench "part2" $ whnf part2 adaps
        ]
    ]

test1 = do
  adaps <- parseTestLines 10 1 number
  guard $ part1 adaps == 35

  print "ok"

test2 = do
  adaps <- parseTestLines 10 2 number
  guard $ part1 adaps == 220
  print "ok"

test3 = do
  adaps <- parseTestLines 10 1 number
  guard $ part2 adaps == 8
  print "ok"

test4 = do
  adaps <- parseTestLines 10 2 number
  guard $ part2 adaps == 19208
  print "ok"