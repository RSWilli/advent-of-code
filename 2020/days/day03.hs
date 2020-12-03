import Bench
import qualified Data.Map as M
import InputParser

slope :: Int -> Int -> Int -> Int -> [Pos]
slope width height xdiff ydiff = take height $ map (\i -> ((xdiff * i) `mod` width, ydiff * i)) [0 ..]

isTree :: Positions -> Pos -> Bool
isTree mountain pos = Just '#' == M.lookup pos mountain

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
  defaultMain
    [ bgroup
        "run"
        [ bench "part1" $ whnf part1 mountain,
          bench "part2" $ whnf part2 mountain
        ]
    ]