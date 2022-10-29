{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Data.List (nub)
import qualified Data.Map as M
import InputParser
import Util

type Rule = (String, [(Int, String)])

bagNameParser :: Parser String
bagNameParser = manyTill (satisfy (\x -> isAlpha x || isSpace x)) (try " bags" <|> " bag")

bagAmountParser :: Parser (Int, String)
bagAmountParser = (,) <$> (decimal <* " ") <*> bagNameParser

containsListParser :: Parser [(Int, String)]
containsListParser = bagAmountParser `sepBy` ", "

containsNothingParser :: Parser [a]
containsNothingParser = [] <$ "no other bags"

ruleParser :: Parser Rule
ruleParser = (,) <$> (bagNameParser <* " contain ") <*> (try containsNothingParser <|> containsListParser) <* "."

part1 :: [Rule] -> Int
part1 rules =
  let graph = M.fromListWith (++) $ concatMap (\(start, edges) -> map (\(_, end) -> (end, [start])) edges) rules
      countBags = concatMap (\name -> name : countBags (M.findWithDefault [] name graph))
   in length $ nub $ countBags $ graph M.! "shiny gold"

part2 :: [Rule] -> Int
part2 rules =
  let graph = M.fromList rules
      countBags = sum . map (\(c, name) -> c + c * countBags (graph M.! name))
   in countBags (graph M.! "shiny gold")

main = do
  rules <- parseInputLines 7 ruleParser
  print $ part1 rules
  print $ part2 rules
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 7 ruleParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 rules,
          bench "part2" $ whnf part2 rules
        ]
    ]