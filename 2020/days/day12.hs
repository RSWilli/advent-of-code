{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Char (intToDigit)
import Data.List (foldl')
import InputParser
import Util

type Instruction = (Direction, Int)

data Direction = N | E | S | W | L | R | F | U deriving (Show)

data Position = P (Int, Int) Int Int deriving (Show)

rotate :: Instruction -> Position -> Position
rotate (_, 0) p = p
rotate (_, 360) p = p
rotate (_, 180) (P (vx, vy) x y) = P (- vx, - vy) x y
rotate (R, 90) (P (vx, vy) x y) = P (vy, - vx) x y
rotate (L, 270) (P (vx, vy) x y) = P (vy, - vx) x y
rotate (L, 90) (P (vx, vy) x y) = P (- vy, vx) x y
rotate (R, 270) (P (vx, vy) x y) = P (- vy, vx) x y

manhatten :: Position -> Int
manhatten (P _ x y) = abs x + abs y

step :: Position -> Instruction -> Position
--absolute:
step (P (vx, vy) x y) (N, d) = P (vx, vy + d) x y
step (P (vx, vy) x y) (S, d) = P (vx, vy - d) x y
step (P (vx, vy) x y) (E, d) = P (vx + d, vy) x y
step (P (vx, vy) x y) (W, d) = P (vx - d, vy) x y
step (P (vx, vy) x y) (F, d) = P (vx, vy) (x + d * vx) (y + d * vy)
--relative left turns:
step p i = rotate i p

stepP1 :: Position -> Instruction -> Position
stepP1 (P facing x y) (N, d) = P facing x (y + d)
stepP1 (P facing x y) (E, d) = P facing (x + d) y
stepP1 (P facing x y) (S, d) = P facing x (y - d)
stepP1 (P facing x y) (W, d) = P facing (x - d) y
stepP1 p i = step p i

part1 :: [Instruction] -> Int
part1 ins = manhatten $ foldl stepP1 (P (1, 0) 0 0) ins

part2 :: [Instruction] -> Int
part2 ins = manhatten $ foldl' step (P (10, 1) 0 0) ins

directionParser :: Parser Instruction
directionParser =
  choice
    [ (N,) <$> ("N" *> decimal),
      (E,) <$> ("E" *> decimal),
      (S,) <$> ("S" *> decimal),
      (W,) <$> ("W" *> decimal),
      (L,) <$> ("L" *> decimal),
      (R,) <$> ("R" *> decimal),
      (F,) <$> ("F" *> decimal)
    ]

main = do
  ins <- parseInputLines 12 directionParser
  print $ part1 ins
  print $ part2 ins
  test1
  test2
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 12 directionParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 ins,
          bench "part2" $ whnf part2 ins
        ]
    ]

test1 = do
  ins <- parseTestLines 12 1 directionParser
  guard $ part1 ins == 25
  print "ok"

test2 = do
  ins <- parseTestLines 12 1 directionParser
  guard $ part2 ins == 286
  print "ok"