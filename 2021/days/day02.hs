{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import InputParser

data Instruction = Forward Int | Up Int | Down Int deriving (Show, Eq)

instructionParser :: Parser Instruction
instructionParser =
  choice
    [ Forward <$> (lexeme "forward" *> number),
      Up <$> (lexeme "up" *> number),
      Down <$> (lexeme "down" *> number)
    ]

driveSubmarineSimple :: [Instruction] -> Pos
driveSubmarineSimple = foldl step (0, 0)
  where
    step (x, depth) i = case i of
      Forward n -> (x + n, depth)
      Up n -> (x, depth - n)
      Down n -> (x, depth + n)

driveSubmarine :: [Instruction] -> (Int, Int, Int)
driveSubmarine = foldl step (0, 0, 0)
  where
    step (x, depth, aim) i = case i of
      Forward n -> (x + n, depth + aim * n, aim)
      Up n -> (x, depth, aim - n)
      Down n -> (x, depth, aim + n)

part1 :: [Instruction] -> Int
part1 xs = let (x, d) = driveSubmarineSimple xs in x * d

part2 :: [Instruction] -> Int
part2 xs = let (x, d, _) = driveSubmarine xs in x * d

main :: IO ()
main = do
  test1
  test2

  input <- parseInputLines 2 instructionParser
  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 2 instructionParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTestLines 2 1 instructionParser
  guard $ part1 input == 150
  print ("ok" :: [Char])

test2 :: IO ()
test2 = do
  list <- parseTestLines 2 1 instructionParser
  guard $ part2 list == 900
  print ("ok" :: [Char])
