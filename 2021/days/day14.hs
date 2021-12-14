{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics (Generic)
import InputParser
import TwoD (Pos, print2D)

type Insertions = M.HashMap (Char, Char) Char

inputParser :: Parser (String, Insertions)
inputParser = (,) <$> text <* "\n\n" <*> (M.fromList <$> polymerParser `sepBy` "\n")

polymerParser :: Parser ((Char, Char), Char)
polymerParser = (,) <$> ((,) <$> letterChar <*> letterChar) <* " -> " <*> letterChar

step :: String -> Insertions -> String
step = go
  where
    go [] _ = []
    go [x] _ = [x]
    go (x : y : xs) m = case M.lookup (x, y) m of
      Just z -> x : z : go (y : xs) m
      Nothing -> x : y : go xs m

run :: Int -> String -> Insertions -> String
run 0 s _ = s
run n s m = run (n - 1) (step s m) m

getScore :: String -> Int
getScore s = score
  where
    counts = foldr (\x -> M.insertWith (+) x 1) M.empty s
    (lo, hi) = M.foldr (\v (mini, maxi) -> (mini `min` v, maxi `max` v)) (maxBound, minBound) counts
    score = hi - lo

part1 :: (String, Insertions) -> Int
part1 (str, ins) = getScore $ run 10 str ins

-- part2 = S.toList . uncurry applyFolds

main :: IO ()
main = do
  test1

  input <- parseInput 14 inputParser

  print (part1 input)

-- print (part2 input)

-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInput 14 inputParser)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 input,
--         bench "part2" $ whnf part2 input
--       ]
--   ]

test1 :: IO ()
test1 = do
  input <- parseTest 14 1 inputParser
  print $ part1 input
  guard $ part1 input == 1588
  print ("ok" :: String)