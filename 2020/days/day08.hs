{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Data.Either (isRight)
import qualified Data.IntMap as M
import Data.List (nub)
import qualified Data.Set as S
import InputParser
import Util

data Op = Nop Int | Jump Int | Acc Int deriving (Show)

type Program = [Op]

data Machine = Machine
  { pc :: Int,
    acc :: Int,
    program :: M.IntMap Op
  }

createMachine :: Program -> Machine
createMachine prog = Machine {pc = 0, acc = 0, program = M.fromList $ zip [0 ..] prog}

createHistory mach = (S.empty, mach)

step :: Machine -> Maybe Machine
step m = run operation
  where
    mem = program m
    addr = pc m
    accu = acc m
    operation = mem M.!? addr
    run Nothing = Nothing
    run (Just (Nop _)) = Just $ m {pc = addr + 1}
    run (Just (Jump x)) = Just $ m {pc = addr + x}
    run (Just (Acc x)) = Just $ m {pc = addr + 1, acc = accu + x}

type RunHistory = (S.Set Int, Machine)

-- left = ran into loop
-- right = reached end
runTillEnd :: RunHistory -> Either RunHistory RunHistory
runTillEnd (h, m) = maybe (Right (h, m)) go $ step m
  where
    go newm =
      if S.member (pc newm) h
        then Left (h, newm)
        else runTillEnd (S.insert (pc newm) h, newm)

part1 :: Program -> Int
part1 = either (acc . snd) (error "huh?") . runTillEnd . createHistory . createMachine

opParser :: Parser Op
opParser =
  choice
    [ Nop <$> ("nop" *> space *> number),
      Jump <$> ("jmp" *> space *> number),
      Acc <$> ("acc" *> space *> number)
    ]

createPrograms :: Program -> [Program]
createPrograms = tail . foldr createOption [[]]
  where
    createOption op@(Acc _) ops = map (op :) ops
    createOption op@(Nop x) (real : rest) = (op : real) : (Jump x : real) : map (op :) rest
    createOption op@(Jump x) (real : rest) = (op : real) : (Nop x : real) : map (op :) rest

part2 :: Program -> Int
part2 prog =
  let progs = createPrograms prog
      machs = map (runTillEnd . createHistory . createMachine) progs
   in either (\x -> error "huh?") (acc . snd) $ head $ filter isRight machs

main = do
  ops <- parseInputLines 8 opParser
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