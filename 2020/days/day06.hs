{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.Set as S
import InputParser
import Util

groupParser :: Parser [S.Set Char]
groupParser = map S.fromList <$> name `sepEndBy` "\n"

groupsParser :: Parser [[S.Set Char]]
groupsParser = groupParser `sepBy` "\n"

part1 :: [[S.Set Char]] -> Int
part1 = sum . map (S.size . S.unions)

part2 :: [[S.Set Char]] -> Int
part2 = sum . map (\(x : xs) -> S.size $ foldl S.intersection x xs)

main = do
  groups <- parseInput 6 groupsParser
  print $ part1 groups
  print $ part2 groups
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 6 groupsParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 groups,
          bench "part2" $ whnf part2 groups
        ]
    ]