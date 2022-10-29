{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (intToDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import InputParser
import Numeric (showIntAtBase)
import Util

showBin x = showIntAtBase 2 intToDigit x ""

data Instruction = Mask String | Mem Int Int deriving (Show)

type State = (String, IntMap Int)

memParser :: Parser Instruction
memParser = Mem <$ "mem[" <*> decimal <* "] = " <*> decimal

maskParser :: Parser Instruction
maskParser = Mask <$ "mask = " <*> many (oneOf ['1', '0', 'X'])

instructionParser :: Parser Instruction
instructionParser = try maskParser <|> memParser

type Mask = (Int, Int)

type MemMask = (Int, [Int])

toMask :: String -> Mask
toMask =
  foldl
    ( \(and, or) c -> case c of
        'X' -> (and * 2 + 1, or * 2)
        '0' -> (and * 2, or * 2)
        '1' -> (and * 2, or * 2 + 1)
        _ -> error "unknown char"
    )
    (0, 0)

executeMask :: [Instruction] -> State
executeMask ((Mem _ _) : ins) = error "mask needs to be specified first"
executeMask ((Mask m) : ins) =
  foldl
    ( \(mask, mem) ins -> case ins of
        Mask m -> (m, mem)
        Mem addr v -> let (and, or) = toMask m in (mask, M.insert addr ((v .&. and) .|. or) mem)
    )
    (m, M.empty)
    ins

part1 :: [Instruction] -> Int
part1 = sum . snd . executeMask

part2 :: [Instruction] -> Int
part2 = sum . snd . executeMemMap

executeMemMap :: [Instruction] -> State
executeMemMap ((Mem _ _) : ins) = error "mask needs to be specified first"
executeMemMap ((Mask m) : ins) =
  foldl
    ( \(mask, mem) ins -> case ins of
        Mask m -> (m, mem)
        Mem addr v ->
          let addrs = applyMemMask addr mask
           in (mask, foldr (`M.insert` v) mem addrs)
    )
    (m, M.empty)
    ins

enumFloating :: String -> [Int]
enumFloating mask =
  foldl
    (\masks -> concatMap $ \x -> map (\i -> 2 * i + x) masks)
    [0]
    $ map
      ( \case
          '0' -> [0]
          '1' -> [1]
          'X' -> [1, 0]
      )
      mask

toMemMask :: String -> MemMask
toMemMask mask =
  let (m, floating) =
        foldl
          ( \(mask, floating) c -> case c of
              '0' -> (2 * mask, floating ++ "0")
              '1' -> (2 * mask + 1, floating ++ "0")
              'X' -> (2 * mask, floating ++ "X")
          )
          (0, "")
          mask
   in (m, enumFloating floating)

applyMemMask :: Int -> String -> [Int]
applyMemMask n m =
  let (mask, floatings) = toMemMask m
      base = n .|. mask
   in map (xor base) floatings

main = do
  ins <- parseInputLines 14 instructionParser
  print $ part1 ins
  print $ part2 ins
  test1
  test2
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 14 instructionParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 ins,
          bench "part2" $ whnf part2 ins
        ]
    ]

test1 = do
  ins <- parseTestLines 14 1 instructionParser
  guard $ part1 ins == 165
  print "ok"

test2 = do
  ins <- parseTestLines 14 2 instructionParser
  guard $ part2 ins == 208
  print "ok"
