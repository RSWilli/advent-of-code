{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Ix
import InputParser
import Prelude hiding (subtract)

type Pos3d = (Int, Int, Int)

data Instruction = On Pos3d Pos3d | Off Pos3d Pos3d deriving (Show, Eq)

data Cube = Cube Pos3d Pos3d deriving (Show, Eq)

-- on x=-18..26,y=-33..15,z=-7..46
-- off x=-40..-22,y=-38..-28,z=23..41
instructionParser :: Parser Instruction
instructionParser =
  choice
    [ instruction On <$ "on x=" <*> number <* ".." <*> number <* ",y=" <*> number <* ".." <*> number <* ",z=" <*> number <* ".." <*> number,
      instruction Off <$ "off x=" <*> number <* ".." <*> number <* ",y=" <*> number <* ".." <*> number <* ",z=" <*> number <* ".." <*> number
    ]
  where
    instruction :: (Pos3d -> Pos3d -> Instruction) -> Int -> Int -> Int -> Int -> Int -> Int -> Instruction
    instruction f minx maxx miny maxy minz maxz = f (minx, miny, minz) (maxx, maxy, maxz)

boundsPart1 :: (Pos3d, Pos3d)
boundsPart1 = ((-50, -50, -50), (50, 50, 50))

filterPart1 :: [Instruction] -> [Instruction]
filterPart1 = filter f
  where
    f (On lo hi) = inRange boundsPart1 lo && inRange boundsPart1 hi
    f (Off lo hi) = inRange boundsPart1 lo && inRange boundsPart1 hi

difference :: Cube -> Cube -> [Cube]
difference c1 c2 = case intersection c1 c2 of
  Nothing -> [c1]
  Just cu -> subtract c1 cu

valid :: Cube -> Bool
valid (Cube (x1, y1, z1) (x2, y2, z2)) =
  (x1 <= x2) && (y1 <= y2) && (z1 <= z2)

subtract :: Cube -> Cube -> [Cube]
subtract (Cube (x1, y1, z1) (x2, y2, z2)) (Cube (x3, y3, z3) (x4, y4, z4)) =
  filter
    valid
    [ Cube (x1, y1, z1) (x2, y2, z3 - 1), --top
      Cube (x1, y1, z4 + 1) (x2, y2, z2), --bottom
      Cube (x1, y1, z3) (x3 - 1, y2, z4), --left
      Cube (x4 + 1, y1, z3) (x2, y2, z4), --right
      Cube (x3, y1, z3) (x4, y3 - 1, z4), --front
      Cube (x3, y4 + 1, z3) (x4, y2, z4) --back
    ]

intersection :: Cube -> Cube -> Maybe Cube
intersection (Cube (x1, y1, z1) (x2, y2, z2)) (Cube (x3, y3, z3) (x4, y4, z4)) =
  let i = Cube (max x1 x3, max y1 y3, max z1 z3) (min x2 x4, min y2 y4, min z2 z4)
   in if valid i
        then Just i
        else Nothing

volume :: Cube -> Int
volume (Cube (x1, y1, z1) (x2, y2, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

applyInstructions :: [Instruction] -> [Cube]
applyInstructions = foldl apply []
  where
    apply :: [Cube] -> Instruction -> [Cube]
    apply cubes (On lo hi) = Cube lo hi : (cubes >>= (`difference` Cube lo hi))
    apply cubes (Off lo hi) = cubes >>= (`difference` Cube lo hi)

part1 :: [Instruction] -> Int
part1 = sum . map volume . applyInstructions . filterPart1

part2 :: [Instruction] -> Int
part2 = sum . map volume . applyInstructions

main :: IO ()
main = do
  test1

  test2

  input <- parseInputLines 22 instructionParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 22 instructionParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTestLines 22 1 instructionParser
  guard $ part1 input == 590784
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTestLines 22 2 instructionParser
  guard $ part2 input == 2758514936282235
  print ("ok" :: String)