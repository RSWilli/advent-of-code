{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad (guard)
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S
import InputParser
import Util

-- ingredientParser :: Parser _
ingredientParser = (,) <$> manyTill (text <* " ") "(contains " <*> (text `sepBy` ", " <* ")")

main = do
  --   ingredients <- parseInputLines 21 ingredientParser
  -- print $ part1 messages rules
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
  ingredients <- parseTestLines 21 1 ingredientParser
  print ingredients
  -- guard $ part1 messages rules == 2
  print "ok"
