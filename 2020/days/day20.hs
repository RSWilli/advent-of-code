{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Bits
import qualified Data.Vector as V
import InputParser
import Util

-- n e s w
data Tile = Tile Int Int Int Int deriving (Show)

type TileID = (Int, Tiles)

type Tiles = [Tile]

size = 10

toInt :: [Char] -> Int
toInt = foldr (\c n -> 2 * n + (if c == '#' then 1 else 0)) 0

toIntRev :: [Char] -> Int
toIntRev = toInt . reverse

parseTile :: Parser Tiles
parseTile = do
  res <- manyTill (some printChar <* "\n") "\n"
  let tilemap = V.fromList $ map V.fromList res

  let top = [(x, 0) | x <- [0 .. size - 1]]
  let right = [(size - 1, y) | y <- [0 .. size - 1]]
  let bottom = [(x, size - 1) | x <- [0 .. size - 1]]
  let left = [(0, y) | y <- [0 .. size - 1]]

  let tops = map (lookup2D tilemap) top
  let bottoms = map (lookup2D tilemap) bottom
  let lefts = map (lookup2D tilemap) left
  let rights = map (lookup2D tilemap) right

  let a = toInt tops
  let ai = toIntRev tops
  let b = toInt rights
  let bi = toIntRev rights
  let c = toInt bottoms
  let ci = toIntRev bottoms
  let d = toInt lefts
  let di = toIntRev lefts

  return
    [ Tile a b c d, -- A
      Tile di a bi c, -- Ar1
      Tile ci di ai bi, -- Ar2
      Tile b ci d ai, -- Ar3
      Tile ai d ci b, -- Am
      Tile bi ai di ci, -- Amr1
      Tile c bi a di, -- Amr2
      Tile d c b a -- Amr3
    ]

tileIDParser :: Parser TileID
tileIDParser = (,) <$> ("Tile " *> decimal <* ":\n") <*> parseTile

tilesParser :: Parser [TileID]
tilesParser = many tileIDParser

main = do
  tiles <- parseInput 20 tilesParser
  -- print $ part1 messages rules
  -- print messages

  test1

-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInput 17)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 values,
--         bench "part2" $ whnf part2 values
--       ]
--   ]

test1 = do
  tiles <- parseTest 20 1 tilesParser
  print tiles
  -- guard $ part1 messages rules == 2
  print "ok"
