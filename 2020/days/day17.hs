{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sortOn, transpose)
import qualified Data.Set as S
import InputParser
import Util

type Pos3D = (Int, Int, Int)

type Pos4D = (Int, Int, Int, Int)

type Spacial = S.Set Pos3D

type Hyper = S.Set Pos4D

neighbors3d :: Pos3D -> [Pos3D]
neighbors3d (x, y, z) = do
  nx <- [x - 1 .. x + 1]
  ny <- [y - 1 .. y + 1]
  nz <- [z - 1 .. z + 1]
  guard $ nx /= x || ny /= y || nz /= z
  return (nx, ny, nz)

neighbors4d :: Pos4D -> [Pos4D]
neighbors4d (x, y, z, w) = do
  nx <- [x - 1 .. x + 1]
  ny <- [y - 1 .. y + 1]
  nz <- [z - 1 .. z + 1]
  nw <- [w - 1 .. w + 1]
  guard $ nx /= x || ny /= y || nz /= z || nw /= w
  return (nx, ny, nz, nw)

countAlive :: (Ord a) => [a] -> S.Set a -> Int
countAlive poses space = length $ filter (`S.member` space) poses

allNeighbors :: (Ord a) => (a -> [a]) -> S.Set a -> S.Set a
allNeighbors neighfn space = S.fromList (S.toList space >>= neighfn) S.\\ space

step :: (Ord a) => (a -> [a]) -> S.Set a -> S.Set a
step neighbors actives =
  let inactives = allNeighbors neighbors actives
      actives' = S.filter (\pos -> let alive = countAlive (neighbors pos) actives in alive == 3 || alive == 2) actives
      newactives = S.filter (\pos -> countAlive (neighbors pos) actives == 3) inactives
   in S.union actives' newactives

toSpacial :: String -> Spacial
toSpacial input =
  let ls = lines input
      poses =
        concatMap (\(y, row) -> map (\(x, c) -> ((x, y, 0), c)) row) $
          zip [0 ..] $ map (zip [0 ..]) ls
   in S.fromList $ map fst $ filter ((== '#') . snd) poses

toHyper :: String -> Hyper
toHyper input =
  let ls = lines input
      poses =
        concatMap (\(y, row) -> map (\(x, c) -> ((x, y, 0, 0), c)) row) $
          zip [0 ..] $ map (zip [0 ..]) ls
   in S.fromList $ map fst $ filter ((== '#') . snd) poses

part1 :: Spacial -> Int
part1 space = S.size $ step3d $ step3d $ step3d $ step3d $ step3d $ step3d space
  where
    step3d = step neighbors3d

part2 :: Hyper -> Int
part2 space = S.size $ step4d $ step4d $ step4d $ step4d $ step4d $ step4d space
  where
    step4d = step neighbors4d

main = do
  input <- readFile (getInputPath 17)

  print $ part1 $ toSpacial input
  print $ part2 $ toHyper input

  test1
  test2

-- defaultMain
--   [ bgroup
--       "parse"
--       [ bench "input" $ whnfIO (parseInput 17)
--       ],
--     bgroup
--       "run"
--       [ bench "part1" $ whnf part1 game,
--         bench "part2" $ whnf part2 game
--       ]
--   ]

test1 = do
  game <- toSpacial <$> readFile (getTestPath 17 1)
  guard $ part1 game == 112
  print "ok"

test2 = do
  game <- toHyper <$> readFile (getTestPath 17 1)
  guard $ part2 game == 848
  print "ok"
