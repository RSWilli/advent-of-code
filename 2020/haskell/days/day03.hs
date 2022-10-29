import Bench
import Control.Monad (guard)
import qualified Data.Map as M
import InputParser

slope :: Int -> Int -> Int -> Int -> [Pos]
slope width height xdiff ydiff = take (height `div` ydiff) $ map (\i -> ((xdiff * i) `mod` width, ydiff * i)) [0 ..]

isTree :: Positions -> Pos -> Bool
isTree mountain pos = '#' == lookup2D mountain pos

countTrees :: Positions -> [Pos] -> Int
countTrees mountain slope = length $ filter (isTree mountain) slope

part1 mountain =
  let (width, height) = dimensions mountain
   in countTrees mountain $ slope width height 3 1

part2 mountain =
  let (width, height) = dimensions mountain
      s = slope width height
   in product $ map (countTrees mountain) [s 1 1, s 3 1, s 5 1, s 7 1, s 1 2]

main = do
  mountain <- parseInput2D 3
  print $ part1 mountain
  print $ part2 mountain
  test1
  test2
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput2D 3)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 mountain,
          bench "part2" $ whnf part2 mountain
        ]
    ]

test1 = do
  mountain <- parseTest2D 3 1
  guard $ part1 mountain == 7
  print "ok"

test2 = do
  mountain <- parseTest2D 3 1
  guard $ part2 mountain == 336
  print "ok"