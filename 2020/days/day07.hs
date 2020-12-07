{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Tree as T
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
  let edgeList = map (\(v, edges) -> (v, v, map snd edges)) rules
      (graph, vertexToKey, keyToVertex) = G.graphFromEdges edgeList
      Just start = keyToVertex "shiny gold"
   in length (G.reachable (G.transposeG graph) start) - 1

part2 :: [Rule] -> Int
part2 rules =
  let graph = M.fromList rules
      countBags = sum . map (\(c, name) -> c + c * countBags (graph M.! name))
   in countBags (graph M.! "shiny gold")

main = do
  rules <- parseInputLines 7 ruleParser
  -- putStrLn $ unlines $ map show rules
  print $ part1 rules
  print $ part2 rules

-- print $ part1 groups
-- print $ part2 groups
-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInput 6 groupsParser)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 groups,
--         bench "part2" $ whnf part2 groups
--       ]
--   ]