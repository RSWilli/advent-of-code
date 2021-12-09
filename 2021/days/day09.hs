{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import TwoD

type Field = TwoD Int

floodFill :: Field -> Pos -> Int
floodFill f seed = go [seed] S.empty
  where
    go [] done = S.size done
    go (t : odo) done =
      let done' = S.insert t done
          todo' = odo ++ filter (\p -> (9 /= lookup2DWithDefault 9 f p) && not (p `S.member` done)) (neighs t)
       in go todo' done'

neighbors :: Field -> (Int, Int) -> [Int]
neighbors positions = map (fromMaybe 9 . lookup2D positions) . neighs

computeRiskLevel :: Field -> Field
computeRiskLevel positions = map2D (\p v -> if 0 > maximum (map (v -) $ neighbors positions p) then v + 1 else 0) positions

part1 :: Field -> Int
part1 = fold2D (+) 0 . computeRiskLevel

part2 :: Field -> Int
part2 f =
  let rl = computeRiskLevel f
      pockets = ifold2D (\pos xs v -> if v == 0 then xs else floodFill f pos : xs) [] rl
   in product $ take 3 $ sortBy (flip compare) pockets

main :: IO ()
main = do
  test1

  test2

  input <- parseInput2D 9 digitToInt

  print (part1 input)
  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput2D 9 digitToInt)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest2D 9 1 digitToInt
  guard $ part1 input == 15
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest2D 9 1 digitToInt
  guard $ part2 input == 1134
  print ("ok" :: String)
