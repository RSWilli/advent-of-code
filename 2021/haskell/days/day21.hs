{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Bench
import Control.Monad (forM, guard)
import Control.Monad.State (MonadState (get, put), State, evalState)
import InputParser

playerParser :: Parser Int
playerParser = "Player " *> (decimal :: Parser Int) *> " starting position: " *> decimal

inputParser :: Parser (Int, Int)
inputParser = (,) <$> playerParser <* "\n" <*> playerParser

data GameState = GS
  { dice :: Int,
    rolls :: Int,
    position1 :: Int,
    position2 :: Int,
    score1 :: Int,
    score2 :: Int --,
    -- cache :: M.Map (Int, Int, Int, Int) (Int, Int)
  }
  deriving (Show)

type Dice = State GameState

type DiceCounts = [(Int, Int)] -- sum of rolled dice plus respective counts

rollDeterministic :: Dice DiceCounts
rollDeterministic = do
  s <- get
  let c = dice s
  put $ s {dice = (c + 3) `mod` 100, rolls = rolls s + 3}
  let n1 = c `mod` 100
  let n2 = (c + 1) `mod` 100
  let n3 = (c + 2) `mod` 100
  let res = n1 + n2 + n3 + 3 -- the dice ranges 1-100, but the state represents 0-99, so we need to add 3
  return [(res, 1)] -- this result can only happen once

rollDirac :: Dice DiceCounts
rollDirac = return [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

setPlayer1 :: Int -> Int -> Dice ()
setPlayer1 p s = do
  state <- get
  put $ state {position1 = p, score1 = s}

setPlayer2 :: Int -> Int -> Dice ()
setPlayer2 p s = do
  state <- get
  put $ state {position2 = p, score2 = s}

type Player = Dice DiceCounts -> Dice (Int, (Int, Int))

step :: (GameState -> Int) -> (GameState -> Int) -> (GameState -> Int) -> (Int -> Int -> Dice ()) -> Player -> (Int -> (Int, Int)) -> Int -> Player
step getPos getScore getOtherScore updatePlayer other count maxscore roll = do
  ns <- roll
  scores <- forM ns $ \(n, c) -> do
    state <- get
    let r = rolls state
    let pos = ((getPos state + n - 1) `mod` 10) + 1
    let score = getScore state + pos
    updatePlayer pos score
    if score >= maxscore
      then do
        put state
        return (getOtherScore state * r, count c)
      else do
        (s, (c', c'')) <- other roll
        put state
        return (s, (c' * c, c'' * c))
  -- ignore summing the scores, since its only needed for part 1
  let res = foldl1 (\(s, (sumlc, sumrc)) (_, (lc, rc)) -> (s, (sumlc + lc, sumrc + rc))) scores
  return res

player1 :: Int -> Player
player1 x = step position1 score1 score2 setPlayer1 (player2 x) (,0) x

player2 :: Int -> Player
player2 x = step position2 score2 score1 setPlayer2 (player1 x) (0,) x

play :: Dice DiceCounts -> Int -> (Int, Int) -> (Int, (Int, Int))
play roll x (p1, p2) = evalState (player1 x roll) (GS 0 0 p1 p2 0 0)

part1 :: (Int, Int) -> Int
part1 = fst . play rollDeterministic 1000

part2 :: (Int, Int) -> Int
part2 = uncurry max . snd . play rollDirac 21

main :: IO ()
main = do
  test1

  test2

  input <- parseInput 21 inputParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 21 inputParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 21 1 inputParser
  guard $ part1 input == 739785
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 21 1 inputParser
  guard $ part2 input == 444356092776315
  print ("ok" :: String)