{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.IntMap as M
import Data.List (nub)
import qualified Data.Set as S
import InputParser
import Util

data Op = Nop | Jump Int | Acc Int deriving (Show)

type Program = M.IntMap Op

data Machine = Machine
  { pc :: Int,
    acc :: Int,
    program :: Program
  }

createMachine :: Program -> Machine
createMachine prog = Machine {pc = 0, acc = 0, program = prog}

createHistory mach = (S.empty, mach)

step :: Machine -> Machine
step m = run operation
  where
    mem = program m
    addr = pc m
    accu = acc m
    operation = mem M.! addr
    run Nop = m {pc = addr + 1}
    run (Jump x) = m {pc = addr + x}
    run (Acc x) = m {pc = addr + 1, acc = accu + x}

type RunHistory = (S.Set Int, Machine)

findLoop :: RunHistory -> RunHistory
findLoop (h, m) =
  let newm = step m
   in if S.member (pc newm) h
        then (h, newm)
        else findLoop (S.insert (pc newm) h, newm)

part1 :: Program -> Int
part1 = acc . snd . findLoop . createHistory . createMachine

opParser :: Parser Op
opParser =
  choice
    [ Nop <$ ("nop" *> space *> number),
      Jump <$> ("jmp" *> space *> number),
      Acc <$> ("acc" *> space *> number)
    ]

main = do
  ops <- M.fromList . zip [0 ..] <$> parseInputLines 8 opParser
  print $ part1 ops

-- print $ part2 rules
-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInputLines 7 ruleParser)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 rules,
--         bench "part2" $ whnf part2 rules
--       ]
--   ]