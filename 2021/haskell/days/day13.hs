{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.HashSet as S
import Data.Hashable
import GHC.Generics (Generic)
import InputParser
import TwoD (Pos, print2D)

data Instruction = X Int | Y Int deriving (Eq, Show, Generic)

type Image = S.HashSet Pos

--https://hackage.haskell.org/package/hashable-1.4.0.0/docs/Data-Hashable.html#g:4
instance Hashable Instruction

instructionParser :: Parser Instruction
instructionParser = "fold along " *> (X <$ "x" <|> Y <$ "y") <* "=" <*> number

posParser :: Parser Pos
posParser = (,) <$> decimal <* "," <*> decimal

imageParser :: Parser Image
imageParser = S.fromList <$> posParser `sepEndBy` "\n"

inputParser :: Parser (Image, [Instruction])
inputParser = (,) <$> imageParser <* "\n" <*> instructionParser `sepBy` "\n"

applyFolds :: Image -> [Instruction] -> Image
applyFolds = foldl foldAlong
  where
    foldAlong image (X n) = S.map (\(x, y) -> (if x > n then 2 * n - x else x, y)) image
    foldAlong image (Y n) = S.map (\(x, y) -> (x, if y > n then 2 * n - y else y)) image

part1 :: (Image, [Instruction]) -> Int
part1 (image, instr) = S.size $ applyFolds image $ take 1 instr

part2 :: (Image, [Instruction]) -> [Pos]
part2 = S.toList . uncurry applyFolds

main :: IO ()
main = do
  test1

  input <- parseInput 13 inputParser

  print (part1 input)

  print2D (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 13 inputParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 13 1 inputParser
  print2D $ S.toList $ uncurry applyFolds input
  guard $ part1 input == 17
  print ("ok" :: String)