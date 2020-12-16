{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.List (isPrefixOf, transpose)
import InputParser
import Util

type Rule = Int -> Bool

type Rules = [(String, Rule, Rule)]

type Ticket = [Int]

type Tickets = (Rules, Ticket, [Ticket])

rangeParser :: Parser Rule
rangeParser = (<=>) <$> decimal <* "-" <*> decimal

ruleParser :: Parser (String, Rule, Rule)
ruleParser = (,,) <$> manyTill printChar ": " <*> (rangeParser <* " or ") <*> rangeParser

rulesParser :: Parser Rules
rulesParser = ruleParser `sepEndBy` "\n"

ticketParser :: Parser Ticket
ticketParser = decimal `sepEndBy` ","

ticketsParser :: Parser [Ticket]
ticketsParser = ticketParser `sepEndBy` "\n"

allTicketsParser :: Parser Tickets
allTicketsParser = (,,) <$> (rulesParser <* "\nyour ticket:\n") <*> (ticketParser <* "\n\nnearby tickets:\n") <*> ticketsParser

part1 :: Tickets -> Int
part1 ts =
  let (rs, _, tickets) = ts
      rules = concatMap (\(_, r1, r2) -> [r1, r2]) rs
      invalid = filter (\t -> all (\rule -> not $ rule t) rules) $ concat tickets
   in sum invalid

findNames :: [Ticket] -> Rules -> [[String]]
findNames ts rules =
  let columns = transpose ts
   in map
        ( \column ->
            map (\(n, _, _) -> n) $
              filter (\(_, r1, r2) -> all (\field -> r1 field || r2 field) column) rules
        )
        columns

filterValid :: [Ticket] -> Rules -> [Ticket]
filterValid tickets rules =
  let allrules = concatMap (\(_, r1, r2) -> [r1, r2]) rules
   in filter (all (\t -> any (\rule -> rule t) allrules)) tickets

-- part2 :: Tickets -> Int
part2 ts =
  let (rules, mine, tickets) = ts
      ruleNames = map (\(x, _, _) -> x) rules
      departureRules = filter ("departure" `isPrefixOf`) ruleNames
      validTickets = filterValid tickets rules
   in findNames validTickets rules

main = do
  tickets <- parseInput 16 allTicketsParser
  print $ part1 tickets
  print $ part2 tickets

  test1

  test2

--   defaultMain
--     [ bgroup
--         "parse"
--         [ bench "input" $ whnfIO (parseInput 16 allTicketsParser)
--         ],
--       bgroup
--         "run"
--         [ bench "part1" $ whnf part1 tickets,
--           bench "part2" $ whnf part2 tickets
--         ]
--     ]

test1 = do
  tickets <- parseTest 16 1 allTicketsParser
  guard $ part1 tickets == 71
  print "ok"

test2 = do
  tickets <- parseTest 16 2 allTicketsParser
  print $ part2 tickets
  -- guard $ part2 tickets == 175594
  print "ok"
