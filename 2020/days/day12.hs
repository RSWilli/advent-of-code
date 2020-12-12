{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Char (intToDigit)
import InputParser
import Util

type Instruction = (Direction, Int)

data Direction = N | E | S | W | L | R | F | U deriving (Show)

data Position = P Direction Int Int deriving (Show)

rotate :: Instruction -> Position -> Position
rotate (_, 0) p = p
rotate (L, deg) p = rotate (R, (360 - deg) `mod` 360) p
rotate (R, 90) (P N x y) = P E x y
rotate (R, 180) (P N x y) = P S x y
rotate (R, 270) (P N x y) = P W x y
rotate (R, deg) (P E x y) = rotate (R, (deg + 90) `mod` 360) (P N x y)
rotate (R, deg) (P S x y) = rotate (R, (deg + 180) `mod` 360) (P N x y)
rotate (R, deg) (P W x y) = rotate (R, (deg + 270) `mod` 360) (P N x y)
rotate i p = error $ show i ++ " - " ++ show p

manhatten :: Position -> Int
manhatten (P _ x y) = abs x + abs y

step :: Position -> Instruction -> Position
--absolute:
step (P facing x y) (N, d) = P facing x (y + d)
step (P facing x y) (E, d) = P facing (x + d) y
step (P facing x y) (S, d) = P facing x (y - d)
step (P facing x y) (W, d) = P facing (x - d) y
--relative left turns:
step p i@(L, _) = rotate i p
step p i@(R, _) = rotate i p
--relative forwards:
step p@(P dir _ _) (F, d) = step p (dir, d)

run :: Position -> [Instruction] -> Position
run = foldl step

part1 :: [Instruction] -> Int
part1 ins = manhatten $ run (P E 0 0) ins

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
  -- print $ part2 ins
  test1

-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInput2D 11)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 ins,
--         bench "part2" $ whnf part2 seats
--       ]
--   ]

test1 = do
  ins <- parseTestLines 12 1 directionParser
  guard $ part1 ins == 25
  print "ok"