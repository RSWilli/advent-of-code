{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Bench
import Control.Monad (guard)
import qualified Data.Map as M
import InputParser

type Line = (Pos, Pos)

coordParser :: Parser Pos
coordParser = (,) <$> number <*> ("," *> number)

lineParser :: Parser Line
lineParser = (,) <$> coordParser <*> (" -> " *> coordParser)

isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

getLineCoords :: Line -> [Pos]
getLineCoords ((x1, y1), (x2, y2)) = do
  let xstep = signum (x2 - x1)
  let ystep = signum (y2 - y1)
  let len = max (abs (x2 - x1)) (abs (y2 - y1))

  s <- [0 .. len]
  pure (x1 + s * xstep, y1 + s * ystep)

getLineOverlaps :: [Line] -> Int
getLineOverlaps = M.size . M.filter (>= 2) . M.fromListWith (+) . map (,1) . concatMap getLineCoords

part1 :: [Line] -> Int
part1 xs = getLineOverlaps $ filter (\x -> isHorizontal x || isVertical x) xs

part2 :: [Line] -> Int
part2 = getLineOverlaps

main :: IO ()
main = do
  test1
  test2

  input <- parseInputLines 5 lineParser

  print (part1 input)
  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 5 lineParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTestLines 5 1 lineParser
  print $ part1 input
  guard $ part1 input == 5
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTestLines 5 1 lineParser
  guard $ part2 input == 12
  print ("ok" :: [Char])
