{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import TwoD

type Field = TwoD Int

neighbors :: Field -> (Int, Int) -> [Int]
neighbors positions = map (fromMaybe 9 . lookup2D positions) . neighs

computeRiskLevel :: Field -> Field
computeRiskLevel positions = map2D (\p v -> if 0 > maximum (map (v -) $ neighbors positions p) then v + 1 else 0) positions

part1 :: Field -> Int
part1 = fold2D (+) 0 . computeRiskLevel

-- part2 xs = _

main :: IO ()
main = do
  test1

  -- test2

  input <- parseInput2D 9 digitToInt

  print (part1 input)

-- print (part2 input)

-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInputLines 8 sevenSegParser)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 input,
--         bench "part2" $ whnf part2 input
--       ]
--   ]

test1 :: IO ()
test1 = do
  input <- parseTest2D 9 1 digitToInt
  guard $ part1 input == 15
  print ("ok" :: String)

-- test2 :: IO ()
-- test2 = do
--   input <- parseTestLines 8 1 sevenSegParser
--   guard $ part2 input == 61229
--   print ("ok" :: String)
