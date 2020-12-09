{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import qualified Data.IntSet as S
import qualified Data.Vector as V
import InputParser
import Util

toSet :: V.Vector Int -> S.IntSet
toSet = V.foldr S.insert S.empty

sumAll :: (Monad m, Alternative m) => m Int -> m Int
sumAll xs = do
  x <- xs
  y <- xs
  guard $ x /= y
  return $ x + y

findContigousSum :: V.Vector Int -> Int -> [Int]
findContigousSum xs targetSum = do
  let max = V.length xs

  start <- [0 .. max]
  end <- [0 .. max]

  let length = end - start
  guard $ length > 1

  let vec = V.slice start length xs
  guard $ sum vec == targetSum

  let first = V.maximum vec
      last = V.minimum vec

  return $ first + last

checkBounds :: Int -> V.Vector Int -> Int
checkBounds preamble vec = go preamble
  where
    go i =
      let current = vec V.! i
          preceding = V.slice (i - preamble) preamble vec
          sums = toSet $ sumAll preceding
       in if current `S.member` sums
            then go (i + 1)
            else current

main = do
  nums <- V.fromList <$> parseInputLines 9 number
  test1
  test2
  let err = checkBounds 25 nums
  print err
  print $ findContigousSum nums err

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (V.fromList <$> parseInputLines 9 number)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf (checkBounds 25) nums,
          bench "part2" $ whnf (findContigousSum nums) err
        ]
    ]

test1 = do
  nums <- V.fromList <$> parseTestLines 9 1 number
  guard $ checkBounds 5 nums == 127
  print "ok"

test2 = do
  nums <- V.fromList <$> parseTestLines 9 1 number
  guard $ findContigousSum nums 127 == [62]
  print "ok"