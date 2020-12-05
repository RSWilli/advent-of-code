{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Data.Bits (xor)
import qualified Data.Set as S
import InputParser
import Util

seatParser :: Parser Int
seatParser = foldl (\sum i -> sum * 2 + i) 0 <$> many (1 <$ ("B" <|> "R") <|> (0 <$ ("F" <|> "L")))

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 passes =
  let seats = S.fromList passes
   in foldr xor 0 [0 .. (S.findMin seats - 1)]
        `xor` S.foldr xor 0 seats
        `xor` foldr xor 0 [(S.findMax seats + 1) .. 1023]

main = do
  passes <- parseInputLines 5 seatParser
  print passes
  print $ part1 passes
  print $ part2 passes
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 5 seatParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 passes,
          bench "part2" $ whnf part2 passes
        ]
    ]