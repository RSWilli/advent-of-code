{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import InputParser
import Util

type Decks = ([Int], [Int])

deckParser :: Parser Decks
deckParser = (,) <$> ("Player 1:\n" *> number `sepEndBy` "\n") <*> ("\nPlayer 2:\n" *> number `sepEndBy` "\n")

step :: Decks -> Maybe Decks
step ([], _) = Nothing
step (_, []) = Nothing
step (c1 : d1, c2 : d2) = Just $ if c1 < c2 then (d1, d2 ++ [c2, c1]) else (d1 ++ [c1, c2], d2)

run :: Decks -> Decks
run decks = maybe decks run $ step decks

winner :: Decks -> [Int]
winner (x, []) = x
winner ([], x) = x

getScore :: [Int] -> Int
getScore xs = sum $ zipWith (*) [1 ..] $ reverse xs

part1 :: Decks -> Int
part1 decks = getScore $ winner $ run decks

part2 :: Decks -> Int
part2 decks = either getScore getScore $ runRecursive decks S.empty

runRecursive :: Decks -> S.Set Decks -> Either [Int] [Int]
runRecursive decks history
  | S.member decks history = Left $ fst decks
  | otherwise = case decks of
    ([], x) -> Right x
    (x, []) -> Left x
    _ -> runRecursive (stepRecursive decks) (S.insert decks history)

stepRecursive :: Decks -> Decks
stepRecursive (c1 : d1, c2 : d2) =
  let l1 = length d1
      l2 = length d2
   in if l1 < c1 || l2 < c2
        then fromMaybe undefined $ step (c1 : d1, c2 : d2)
        else case runRecursive (take c1 d1, take c2 d2) S.empty of
          Left _ -> (d1 ++ [c1, c2], d2)
          Right _ -> (d1, d2 ++ [c2, c1])

main = do
  decks <- parseInput 22 deckParser
  print $ part1 decks
  print $ part2 decks

  test1
  test2
  test3
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 22 deckParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 decks,
          bench "part2" $ whnf part2 decks
        ]
    ]

test1 = do
  decks <- parseTest 22 1 deckParser
  guard $ part1 decks == 306
  print "ok"

test2 = do
  decks <- parseTest 22 1 deckParser
  guard $ part2 decks == 291
  print "ok"

test3 = do
  decks <- parseTest 22 2 deckParser
  print $ part2 decks
  print "ok"
