{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Control.Monad.State
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import InputParser hiding (angles)
import Util (cartesianProduct, cartesianProductWith)
import Vector
import Prelude hiding (sum)

data Scanner = Scanner
  { ident :: Int,
    beacons :: S.Set V3
  }
  deriving (Eq)

instance Show Scanner where
  show (Scanner i bs) =
    "Scanner "
      <> show i
      <> ": "
      <> "("
      <> show (S.size bs)
      <> ") "

instance Ord Scanner where
  compare a b = compare (ident a) (ident b)

beaconParser :: Parser V3
beaconParser = V3 <$> number <* "," <*> number <* "," <*> number

scannerParser :: Parser Scanner
scannerParser = (\x y -> Scanner x (S.fromList y)) <$> ("--- scanner " *> decimal <* " ---\n") <*> beaconParser `sepEndBy` "\n"

scannersParser :: Parser [[Scanner]]
scannersParser = (scannerRotations <$> scannerParser) `sepBy` "\n"

mapScanner :: (V3 -> V3) -> Scanner -> Scanner
mapScanner f scanner = scanner {beacons = S.map f (beacons scanner)}

rotateXScanner :: Int -> Scanner -> Scanner
rotateXScanner angle = mapScanner (rotateX angle)

rotateYScanner :: Int -> Scanner -> Scanner
rotateYScanner angle = mapScanner (rotateY angle)

rotateZScanner :: Int -> Scanner -> Scanner
rotateZScanner angle = mapScanner (rotateZ angle)

translateScanner :: V3 -> Scanner -> Scanner
translateScanner v = mapScanner (+ v)

scannerRotations :: Scanner -> [Scanner]
scannerRotations scanner =
  let angles = [0, 90, 180, 270]
      vecs =
        [ map (`rotateZScanner` scanner) angles,
          map (`rotateZScanner` rotateXScanner 180 scanner) angles,
          map (`rotateYScanner` rotateXScanner 90 scanner) angles,
          map (`rotateYScanner` rotateXScanner 270 scanner) angles,
          map (`rotateXScanner` rotateYScanner 90 scanner) angles,
          map (`rotateXScanner` rotateYScanner 270 scanner) angles
        ]
   in concat vecs

overlapScanner :: S.Set V3 -> Scanner -> Maybe (V3, S.Set V3)
overlapScanner poses s2 = go matches
  where
    beacons0 = S.toList poses
    beacons1 = S.toList $ beacons s2
    matches = cartesianProduct beacons0 beacons1

    go [] = Nothing
    go ((b0, b1) : bs) =
      let offset = b1 - b0
          s2' = translateScanner (- offset) s2
          intersection = beacons s2' `S.intersection` poses
       in if S.size intersection >= 12
            then Just (offset, beacons s2' `S.union` poses)
            else go bs

findOverlap :: S.Set V3 -> State ([[Scanner]], [[Scanner]], [V3]) (S.Set V3)
findOverlap poses = do
  (failed, todo, offsets) <- get
  case todo of
    [] -> return poses
    t : odo -> do
      let overlaps = mapMaybe (overlapScanner poses) t --test all rotations of the scanner
      case overlaps of
        [] -> do
          -- when no overlap is found, add the scanner to the failed list
          put (t : failed, odo, offsets)
          findOverlap poses
        ((offset, bs) : _) -> do
          -- when an overlap is found, remove the scanner from the todo and continue with the next one
          put (failed, odo, offset : offsets)
          findOverlap bs

overlapAll :: S.Set V3 -> State ([[Scanner]], [[Scanner]], [V3]) ([V3], S.Set V3)
overlapAll initialBeacons = do
  r <- findOverlap initialBeacons
  (failed, _, offsets) <- get
  case failed of
    [] -> return (offsets, r)
    _ -> do
      put ([], failed, offsets) -- if we still couldn't match all, we try again with the new matched beacons
      overlapAll r

overlap :: [[Scanner]] -> ([V3], S.Set V3)
overlap scanners =
  let initial = beacons $ head $ head scanners
   in evalState (overlapAll initial) ([], tail scanners, [V3 0 0 0])

maxmimumManhattanDistance :: [V3] -> Int
maxmimumManhattanDistance xs = maximum $ cartesianProductWith (\x y -> manhattan (x - y)) xs xs

part1 :: [[Scanner]] -> Int
part1 = S.size . snd . overlap

part2 :: [[Scanner]] -> Int
part2 = maxmimumManhattanDistance . fst . overlap

main :: IO ()
main = do
  test1

  test2

  input <- parseInput 19 scannersParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 19 scannersParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 19 1 scannersParser
  guard $ part1 input == 79
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 19 1 scannersParser
  guard $ part2 input == 3621
  print ("ok" :: String)