{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import qualified Data.HashMap.Strict as M
import InputParser
import Util (countAll)

type Counts = M.HashMap (Char, Char) Int

type Insertions = M.HashMap (Char, Char) [(Char, Char)]

inputParser :: Parser (Counts, Insertions)
inputParser = letterCounts <$> text <* "\n\n" <*> polymerParser `sepBy` "\n"
  where
    letterCounts s p = (countAll $ letterPairs s, M.fromList p)
    letterPairs xs = zip xs (tail xs)

polymerParser :: Parser ((Char, Char), [(Char, Char)])
polymerParser = toPolymer <$> letterChar <*> letterChar <* " -> " <*> letterChar
  where
    toPolymer a b c = ((a, b), [(a, c), (c, b)])

step :: Insertions -> M.HashMap (Char, Char) Int -> M.HashMap (Char, Char) Int
step i m = M.fromListWith (+) [(suc, v) | (p, v) <- M.toList m, suc <- M.findWithDefault [] p i]

run :: Int -> Counts -> Insertions -> Counts
run 0 s _ = s
run n c i = run (n - 1) (step i c) i

getScore :: Counts -> Int
getScore s = score
  where
    counts = M.fromListWith (+) $ [v | ((a, b), count) <- M.toList s, v <- [(a, count), (b, count)]]
    hi = maximum counts
    lo = minimum counts
    score = ((hi + 1) `div` 2) - ((lo + 1) `div` 2)

part1 :: (Counts, Insertions) -> Int
part1 (str, ins) = getScore $ run 10 str ins

part2 :: (Counts, Insertions) -> Int
part2 (str, ins) = getScore $ run 40 str ins

main :: IO ()
main = do
  test1
  test2

  input <- parseInput 14 inputParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 14 inputParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 14 1 inputParser
  guard $ part1 input == 1588
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 14 1 inputParser
  guard $ part2 input == 2188189693529
  print ("ok" :: String)