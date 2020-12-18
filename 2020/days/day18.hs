{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Control.Applicative
import Control.Monad.Combinators.Expr
import InputParser
import Util

expr ops = makeExprParser (term ops) ops

term ops = try (parens $ expr ops) <|> (hspace *> decimal <* hspace)

binaryOP name f = InfixL  (f <$ symbol name)
parseMul = binaryOP "*" (*)
parseAdd = binaryOP "+" (+)

part1Table = [[ parseMul, parseAdd ]]
part2Table = [[ parseAdd ],[ parseMul ]]


main = do
  part1 <- parseInputLines 18 $ expr part1Table
  print $ sum part1

  part2 <- parseInputLines 18 $ expr part2Table
  print $ sum part2
  test1
--   test2

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
  values <- parseTestLines 18 1 $ expr part1Table
  guard $ sum values == 13632 + 12240 + 437 + 26
  print "ok"
