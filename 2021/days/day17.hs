{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Ix (Ix (inRange))
import InputParser
import TwoD (Pos)

targetAreaParser :: Parser (Pos, Pos)
targetAreaParser = toBounds <$> ("target area: x=" *> number <* "..") <*> (number <* ", y=") <*> (number <* "..") <*> number
  where
    toBounds x1 x2 y1 y2 = ((x1, y1), (x2, y2))

step :: Pos -> Pos -> (Pos, Pos)
step (vx, vy) (x, y) = ((max 0 (vx - 1), vy - 1), (x + vx, y + vy))

throw :: (Pos, Pos) -> Pos -> Maybe Int
throw b@((_, ymin), (xmax, _)) v0 = go v0 (0, 0) 0
  where
    go v p hi =
      let (v', (x', y')) = step v p
          hi' = max hi y'
       in if inRange b (x', y')
            then Just hi
            else
              if x' > xmax || y' < ymin
                then Nothing
                else go v' (x', y') hi'

throwAll :: (Pos, Pos) -> [Int]
throwAll b@((x0, y0), (x1, y1)) = do
  v0 <- [(vx, vy) | vx <- [0 .. max x0 x1], vy <- [min y0 y1 .. - min y0 y1 + 1]]
  let res = throw b v0
  case res of
    Nothing -> []
    Just hi -> [hi]

part1 :: (Pos, Pos) -> Int
part1 = maximum . throwAll

part2 :: (Pos, Pos) -> Int
part2 = length . throwAll

main :: IO ()
main = do
  test1
  test2

  input <- parseInput 17 targetAreaParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 17 targetAreaParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 17 1 targetAreaParser
  guard $ part1 input == 45
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 17 1 targetAreaParser
  guard $ part2 input == 112
  print ("ok" :: String)