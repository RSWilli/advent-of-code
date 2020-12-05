{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import qualified Data.Set as S
import InputParser
import Util

data SeatIDPart = F | B | R | L deriving (Show)

type BoardingPass = [SeatIDPart]

data Range = Range {offset :: Int, size :: Int}

instance Show Range where
  show (Range x 1) = show x
  show (Range x y) = show y ++ "-" ++ show (x + y)

data Seat = Seat {fb :: Range, lr :: Range} deriving (Show)

getID :: Seat -> Int
getID (Seat (Range fb 1) (Range lr 1)) = fb * 8 + lr
getID _ = error "range to big"

upperHalf :: Range -> Range
upperHalf (Range off s) = let newsize = div s 2 in Range (off + newsize) newsize

lowerhalf :: Range -> Range
lowerhalf (Range off s) = let newsize = div s 2 in Range off newsize

narrowSeat :: Seat -> SeatIDPart -> Seat
narrowSeat (Seat fb lr) B = Seat (upperHalf fb) lr
narrowSeat (Seat fb lr) F = Seat (lowerhalf fb) lr
narrowSeat (Seat fb lr) L = Seat fb (lowerhalf lr)
narrowSeat (Seat fb lr) R = Seat fb (upperHalf lr)

getSeat :: Seat -> BoardingPass -> Seat
getSeat = foldl narrowSeat

seatIDParser :: Parser SeatIDPart
seatIDParser =
  choice
    [ F <$ "F",
      B <$ "B",
      L <$ "L",
      R <$ "R"
    ]

boardingPassParser :: Parser BoardingPass
boardingPassParser = many seatIDParser

part1 :: [BoardingPass] -> Int
part1 passes =
  let startrange = Seat (Range 0 128) (Range 0 8)
   in maximum $ map (getID . getSeat startrange) passes

part2 :: [BoardingPass] -> S.Set Int
part2 passes =
  let startrange = Seat (Range 0 128) (Range 0 8)
      seatIDs = map (getID . getSeat startrange) passes
      possibleSeats = S.fromList [minimum seatIDs .. maximum seatIDs]
   in foldr S.delete possibleSeats seatIDs

main = do
  passes <- parseInputLines 5 boardingPassParser
  print $ part1 passes
  print $ part2 passes
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 5 boardingPassParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 passes,
          bench "part2" $ whnf part2 passes
        ]
    ]