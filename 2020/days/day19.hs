{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Set (Set)
import qualified Data.Set as S
import InputParser
import Util

data Rule = Options [Rule] | Seq [Int] | C Char deriving (Show)

type Messages = Set String

type Rules = IntMap Rule

ruleParser :: Parser Rule
ruleParser =
  choice
    [ C <$> doubleTicks printChar,
      Options <$> (Seq <$> decimal `sepEndBy` " ") `sepBy` "| "
    ]

rulesParser :: Parser Rules
rulesParser = M.fromList <$> ((,) <$> (integer <* ": ") <*> ruleParser) `sepEndBy` "\n"

messageParser :: Parser Messages
messageParser = S.fromList <$> text `sepBy` "\n"

ruleMessageParser :: Parser (Rules, Messages)
ruleMessageParser = (,) <$> rulesParser <* "\n" <*> messageParser

collectPossibleMessages :: Rules -> Int -> Messages
collectPossibleMessages rules rule = S.fromList $ go $ rules M.! rule
  where
    go (C c) = [[c]]
    go (Options opts) = concatMap go opts
    go (Seq seqs) =
      foldr1 (\ls rs -> [l ++ r | l <- ls, r <- rs]) $
        map (\i -> go (rules M.! i)) seqs

part1 :: Messages -> Rules -> Int
part1 mes rs =
  let possible = collectPossibleMessages rs 0
   in S.size $ S.intersection mes possible

main = do
  (rules, messages) <- parseInput 19 ruleMessageParser
  print $ part1 messages rules
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
  (rules, messages) <- parseTest 19 1 ruleMessageParser
  guard $ part1 messages rules == 2
  print "ok"
