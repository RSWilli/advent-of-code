{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import qualified Data.Matrix as M
import InputParser

successorMatrix :: M.Matrix Integer
successorMatrix =
  M.fromLists
    [ [0, 0, 0, 0, 0, 0, 1, 0, 1], -- take all 0s and add them to 6 and 8
      [1, 0, 0, 0, 0, 0, 0, 0, 0], -- shift the 1s to the 0s
      [0, 1, 0, 0, 0, 0, 0, 0, 0], -- shift the 2s to the 1s
      [0, 0, 1, 0, 0, 0, 0, 0, 0], -- shift the 3s to the 2s
      [0, 0, 0, 1, 0, 0, 0, 0, 0], -- shift the 4s to the 3s
      [0, 0, 0, 0, 1, 0, 0, 0, 0], -- shift the 5s to the 4s
      [0, 0, 0, 0, 0, 1, 0, 0, 0], -- shift the 6s to the 5s
      [0, 0, 0, 0, 0, 0, 1, 0, 0], -- shift the 7s to the 6s
      [0, 0, 0, 0, 0, 0, 0, 1, 0] --  shift the 8s to the 7s
    ]

nthSuccessorMatrix :: Integer -> M.Matrix Integer
nthSuccessorMatrix n = case n of
  0 -> M.identity 9
  1 -> successorMatrix
  _ ->
    if odd n
      then let n' = nthSuccessorMatrix $ (n - 1) `div` 2 in M.multStd successorMatrix $ M.multStd n' n'
      else let n' = nthSuccessorMatrix $ n `div` 2 in M.multStd n' n'

lanternfishParser :: Parser (M.Matrix Integer)
lanternfishParser = toMatrix . toTuple <$> (decimal `sepBy` ",")
  where
    toTuple :: [Integer] -> (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
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
    toMatrix :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer) -> M.Matrix Integer
    toMatrix (a, b, c, d, e, f, g, h, i) =
      M.fromList
        1 -- rows
        9 -- cols
        [a, b, c, d, e, f, g, h, i]

simulate :: Integer -> M.Matrix Integer -> M.Matrix Integer
simulate c m = M.multStd m $ nthSuccessorMatrix c

countFishes :: M.Matrix Integer -> Integer
countFishes = sum . M.toList

part1 :: M.Matrix Integer -> Integer
part1 = countFishes . simulate 80

part2 :: M.Matrix Integer -> Integer
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
