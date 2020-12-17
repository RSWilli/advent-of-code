{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.List (isPrefixOf, sortOn, transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import InputParser
import Util

type Rule = Int -> Bool

type Rules = [(String, Rule)]

type Ticket = [Int]

type Tickets = [Ticket]

type AllTickets = (Rules, Ticket, [Ticket])

rangeParser :: Parser (Int, Int)
rangeParser = (,) <$> decimal <* "-" <*> decimal

ruleParser :: Parser Rule
ruleParser = (\(x1, y1) (x2, y2) v -> (x1 <=> y1) v || (x2 <=> y2) v) <$> rangeParser <* " or " <*> rangeParser

nameRuleParser :: Parser (String, Rule)
nameRuleParser = (,) <$> manyTill printChar ": " <*> ruleParser

rulesParser :: Parser Rules
rulesParser = nameRuleParser `sepEndBy` "\n"

ticketParser :: Parser Ticket
ticketParser = decimal `sepEndBy` ","

ticketsParser :: Parser Tickets
ticketsParser = ticketParser `sepEndBy` "\n"

allTicketsParser :: Parser AllTickets
allTicketsParser = (,,) <$> (rulesParser <* "\nyour ticket:\n") <*> (ticketParser <* "\n\nnearby tickets:\n") <*> ticketsParser

part1 :: AllTickets -> Int
part1 ts =
  let (rs, _, tickets) = ts
      rules = map snd rs
      invalid = filter (\t -> not $ any (\rule -> rule t) rules) $ concat tickets
   in sum invalid

findNames :: Tickets -> Rules -> [[String]]
findNames ts rules =
  let columns = transpose ts
   in map
        ( \column ->
            map fst $
              filter (\(_, rule) -> all rule column) rules
        )
        columns

filterValid :: Tickets -> Rules -> Tickets
filterValid tickets rules =
  let allrules = map snd rules
   in filter (all (\t -> any (\rule -> rule t) allrules)) tickets

mapRules :: [[String]] -> M.Map String Int
mapRules possibilities =
  let ordered = sortOn (length . snd) $ zip [0 ..] $ map S.fromList possibilities
   in snd $
        foldl
          ( \(taken, indices) (index, possible) ->
              let v = head $ S.toList $ possible S.\\ taken
                  taken' = S.insert v taken
                  indices' = M.insert v index indices
               in (taken', indices')
          )
          (S.empty, M.empty)
          ordered

part2 :: AllTickets -> Int
part2 ts =
  let (rules, mine, tickets) = ts
      myTicket = V.fromList mine
      ruleNames = map fst rules
      departureRules = filter ("departure" `isPrefixOf`) ruleNames
      validTickets = filterValid tickets rules
      names = findNames validTickets rules
      ruleIndexes = mapRules names
   in product $ map (\dep -> myTicket V.! (ruleIndexes M.! dep)) departureRules

main = do
  tickets <- parseInput 16 allTicketsParser
  print $ part1 tickets
  print $ part2 tickets

  test1
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 16 allTicketsParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 tickets,
          bench "part2" $ whnf part2 tickets
        ]
    ]

test1 = do
  tickets <- parseTest 16 1 allTicketsParser
  guard $ part1 tickets == 71
  print "ok"
