{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Search (dijkstra)
import TwoD (TwoD, dimensions, inBounds, neighs, parseInput2D, parseTest2D)
import qualified TwoD as DD

part1 :: TwoD Int -> Int
part1 f = let (height, width) = dimensions f in fromJust $ dijkstra (filter (inBounds f) . neighs) (\_ t -> f DD.! t) (0, 0) (height - 1, width - 1)

part2 :: TwoD Int -> Int
part2 f =
  let (h, w) = dimensions f
      (h', w') = (h * 5 - 1, w * 5 - 1)
      mapPos (y, x) p = (p - 1 + (y `div` h) + (x `div` w)) `mod` 9 + 1

      virtLookup (y, x) = let v = f DD.! (y `mod` h, x `mod` w) in mapPos (y, x) v
   in fromJust $ dijkstra (filter (inRange ((0, 0), (h', w'))) . neighs) (\_ t -> virtLookup t) (0, 0) (h', w')

main :: IO ()
main = do
  test1
  test2

  input <- parseInput2D 15 digitToInt

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput2D 15 digitToInt)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest2D 15 1 digitToInt
  guard $ part1 input == 40
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest2D 15 1 digitToInt
  print $ part2 input
  guard $ part2 input == 315
  print ("ok" :: String)
