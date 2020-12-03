{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.Map as M
import InputParser

data Policy = Policy Int Int Char String deriving (Show)

-- 8-10 q: qqglqqqqqqqjqmdbq
policyParser = Policy <$> (decimal <* "-") <*> (decimal <* " ") <*> (letterChar <* ": ") <*> name

(<=>) :: Ord a => a -> a -> a -> Bool
x <=> y = \z -> x <= z && z <= y

isCharAtPos :: M.Map Int Char -> Int -> Char -> Bool
isCharAtPos m i c = Just c == M.lookup i m

countLetter :: Char -> String -> Int
countLetter c = length . filter (c ==)

policyCorrectP1 :: Policy -> Bool
policyCorrectP1 (Policy min max char pass) = min <=> max $ countLetter char pass

policyCorrectP2 :: Policy -> Bool
policyCorrectP2 (Policy first second char pass) =
  let passMap = M.fromList $ zip [1 ..] pass
   in isCharAtPos passMap first char /= isCharAtPos passMap second char

part1 :: [Policy] -> Int
part1 = length . filter policyCorrectP1

part2 :: [Policy] -> Int
part2 = length . filter policyCorrectP2

main = do
  input <- parseInputLines 2 policyParser
  print input
  print $ part1 input
  print $ part2 input
  defaultMain
    [ -- bgroup
      --   "parse"
      --   [ bench "input" $ nfIO (parseInputLines 1) number
      --   ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]