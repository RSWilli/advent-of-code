{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.List (transpose)
import InputParser

data Bit = Zero | One
  deriving (Eq)

instance Show Bit where
  show Zero = "0"
  show One = "1"

type Bits = [Bit]

bitParser :: Parser Bit
bitParser =
  choice
    [ Zero <$ "0",
      One <$ "1"
    ]

rowParser :: Parser Bits
rowParser = many bitParser

countBits :: Bits -> (Int, Int)
countBits =
  foldr
    ( \b (ones, zeros) -> case b of
        Zero -> (ones, zeros + 1)
        One -> (ones + 1, zeros)
    )
    (0, 0)

maxBits :: Bits -> Bit
maxBits xs =
  let (ones, zeros) = countBits xs
   in if ones >= zeros
        then One
        else Zero

minBits :: Bits -> Bit
minBits xs =
  let (ones, zeros) = countBits xs
   in if ones < zeros
        then One
        else Zero

bitListToInt :: Bits -> Int
bitListToInt =
  foldl
    ( \acc b ->
        acc * 2 + case b of
          Zero -> 0
          One -> 1
    )
    0

neg :: Bit -> Bit
neg Zero = One
neg One = Zero

pick :: Int -> (Bits -> Bit) -> [Bits] -> Bits
pick _ _ [x] = x
pick p fn xs =
  let bits = transpose xs !! p
      val = fn bits
      filtered = filter (\b -> val == b !! p) xs
   in pick (p + 1) fn filtered

part1 :: [Bits] -> Int
part1 xs =
  let c = transpose xs
      m = map maxBits c
   in bitListToInt m * bitListToInt (map neg m)

part2 :: [Bits] -> Int
part2 xs =
  let co2 = bitListToInt $ pick 0 minBits xs
      oxy = bitListToInt $ pick 0 maxBits xs
   in oxy * co2

main :: IO ()
main = do
  test1
  test2

  input <- parseInputLines 3 rowParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 3 rowParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTestLines 3 1 rowParser
  guard $ part1 input == 198
  print ("ok" :: [Char])

test2 :: IO ()
test2 = do
  list <- parseTestLines 3 1 rowParser
  guard $ part2 list == 230
  print ("ok" :: [Char])
