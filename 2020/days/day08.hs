{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.IntSet as S
import qualified Data.Vector as V
import InputParser
import Util

data Op = Nop Int | Jump Int | Acc Int deriving (Show)

type Program = V.Vector Op

data Step = Halt Int | Loop Int | Go Op deriving (Show)

type State = (Int, Int)

step :: State -> Op -> State
step (acc, pc) (Nop _) = (acc, pc + 1)
step (acc, pc) (Jump x) = (acc, pc + x)
step (acc, pc) (Acc x) = (acc + x, pc + 1)

alt :: State -> Op -> Maybe State
alt state (Jump x) = Just $ step state (Nop x)
alt state (Nop x) = Just $step state (Jump x)
alt _ _ = Nothing

run :: Program -> S.IntSet -> State -> Step
run prog seen (acc, pc)
  | S.member pc seen = Loop acc
  | otherwise = case prog V.!? pc of
    Nothing -> Halt acc
    Just op ->
      let newstate = step (acc, pc) op
       in run prog (S.insert pc seen) newstate

fromStep :: Step -> Maybe Int
fromStep (Loop _) = Nothing
fromStep (Halt acc) = Just acc

runWithChange :: Program -> S.IntSet -> State -> Maybe Int
runWithChange prog seen (acc, pc) = case prog V.!? pc of
  _ | S.member pc seen -> Nothing
  Nothing -> Just acc
  Just op -> do
    let newstate = step (acc, pc) op
        seen' = S.insert pc seen
        run' = run prog seen'
        runWithChange' = runWithChange prog seen'
        alternate = alt (acc, pc) op
    runWithChange' newstate <|> (fromStep =<< (run' <$> alternate))

part1 :: Program -> Step
part1 prog = run prog S.empty (0, 0)

part2 :: Program -> Maybe Int
part2 prog = runWithChange prog S.empty (0, 0)

opParser :: Parser Op
opParser =
  choice
    [ Nop <$> ("nop" *> " " *> number),
      Jump <$> ("jmp" *> " " *> number),
      Acc <$> ("acc" *> " " *> number)
    ]

main = do
  ops <- V.fromList <$> parseInputLines 8 opParser
  print $ part1 ops
  print $ part2 ops
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 8 opParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 ops,
          bench "part2" $ whnf part2 ops
        ]
    ]