{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Either (isRight)
import Data.Foldable (asum, traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import InputParser
import Util

type Rule = Either [[Int]] Char

type Messages = [String]

type Rules = IntMap Rule

ruleParser :: Parser Rule
ruleParser =
  choice
    [ Right <$> doubleTicks printChar,
      Left <$> (decimal `sepEndBy` " ") `sepBy` "| "
    ]

rulesParser :: Parser Rules
rulesParser = M.fromList <$> ((,) <$> (integer <* ": ") <*> ruleParser) `sepEndBy` "\n"

messageParser :: Parser Messages
messageParser = text `sepBy` "\n"

ruleMessageParser :: Parser (Rules, Messages)
ruleMessageParser = (,) <$> rulesParser <* "\n" <*> messageParser

run :: Messages -> Rules -> Int
run mes rs = length $ filter (isRight . myparse (toParser rs)) mes
  where
    toParser rules = löb (createParser <$> rules) M.! 0 *> eof

    createParser (Left opts) = \parsers -> asum [try $ traverse_ (parsers M.!) seqs | seqs <- opts]
    createParser (Right c) = \parsers -> () <$ satisfy (== c)

part1 :: Messages -> Rules -> Int
part1 = run

part2 :: Messages -> Rules -> Int
part2 mes rs =
  let rs' =
        M.insert 8 (Left [[42], [42, 8]]) $
          M.insert 11 (Left [[42, 31], [42, 11, 31]]) rs
   in run mes rs'

main = do
  (rules, messages) <- parseInput 19 ruleMessageParser
  print $ part1 messages rules
  print $ part2 messages rules

  test1
  test2

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
  (rules, messages) <- parseTest 19 1 ruleMessageParser
  guard $ part1 messages rules == 2
  print "ok"

test2 = do
  (rules, messages) <- parseTest 19 2 ruleMessageParser
  print $ part1 messages rules -- == 3
  print $ part2 messages rules -- == 12
  print "ok"
