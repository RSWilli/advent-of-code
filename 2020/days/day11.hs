{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Char (intToDigit)
import qualified Data.Map as M
import InputParser
import Util

type Seats = Positions

neighbors :: Pos -> Seats -> Int
neighbors (x, y) seats =
  let (w, h) = dimensions seats
      checkBounds (x, y) = (0 <=> (w -1)) x && (0 <=> (h -1)) y
   in length $
        filter
          (\x -> checkBounds x && isTaken seats x)
          [ (x -1, y -1),
            (x, y -1),
            (x + 1, y -1),
            (x -1, y),
            (x + 1, y),
            (x -1, y + 1),
            (x, y + 1),
            (x + 1, y + 1)
          ]

los :: Pos -> Int -> Int -> [Pos]
los pos xdiff ydiff = tail $ iterate (\(x, y) -> (x + xdiff, y + ydiff)) pos

visible :: Seats -> [Pos] -> Int
visible seats poses =
  let (w, h) = dimensions seats
      checkBounds (x, y) = (0 <=> (w -1)) x && (0 <=> (h -1)) y
      validPoses = takeWhile checkBounds poses
      firstSeats = map (seats M.!) $ dropWhile (isFloor seats) validPoses
   in case firstSeats of
        [] -> 0
        'L' : _ -> 0
        '#' : _ -> 1

visibleNeighbors :: Pos -> Seats -> Int
visibleNeighbors pos seats =
  sum $
    map
      (visible seats)
      [ los pos (-1) (-1),
        los pos 0 (-1),
        los pos 1 (-1),
        los pos (-1) 0,
        los pos 1 0,
        los pos (-1) 1,
        los pos 0 1,
        los pos 1 1
      ]

isTaken :: Seats -> Pos -> Bool
isTaken m p = (m M.! p) == '#'

isFloor :: Seats -> Pos -> Bool
isFloor m p = (m M.! p) == '.'

stepP1 :: Seats -> Seats
stepP1 seats =
  M.mapWithKey
    ( \pos seat ->
        let takenneighs = neighbors pos seats
         in case seat of
              '.' -> '.'
              'L' -> if takenneighs == 0 then '#' else 'L'
              '#' -> if takenneighs < 4 then '#' else 'L'
    )
    seats

stepP2 :: Seats -> Seats
stepP2 seats =
  M.mapWithKey
    ( \pos seat ->
        let takenneighs = visibleNeighbors pos seats
         in case seat of
              '.' -> '.'
              'L' -> if takenneighs == 0 then '#' else 'L'
              '#' -> if takenneighs < 5 then '#' else 'L'
    )
    seats

runTillStable :: (Seats -> Seats) -> Seats -> Seats
runTillStable stepfn seats =
  let steps = iterate stepfn seats
      successors = zip steps (tail steps)
   in fst $ head $ filter (uncurry (==)) successors

countOccupied :: Seats -> Int
countOccupied = length . filter ((== '#') . snd) . M.toList

part1 :: Seats -> Int
part1 seats = countOccupied $ runTillStable stepP1 seats

part2 :: Seats -> Int
part2 seats = countOccupied $ runTillStable stepP2 seats

main = do
  seats <- parseInput2D 11
  print $ part1 seats
  print $ part2 seats
  test1
  test2
  test3
  test4
  test5

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput2D 11)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 seats,
          bench "part2" $ whnf part2 seats
        ]
    ]

test1 = do
  seats <- parseTest2D 11 1
  guard $ part1 seats == 37
  print "ok"

test2 = do
  seats <- parseTest2D 11 2
  guard $ visibleNeighbors (3, 4) seats == 8
  print "ok"

test3 = do
  seats <- parseTest2D 11 3
  guard $ visibleNeighbors (1, 1) seats == 0
  print "ok"

test4 = do
  seats <- parseTest2D 11 4
  guard $ visibleNeighbors (3, 3) seats == 0
  print "ok"

test5 = do
  seats <- parseTest2D 11 1
  guard $ part2 seats == 26
  print "ok"