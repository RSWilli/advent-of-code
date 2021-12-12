{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative (Alternative (some))
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import InputParser

data Cave = Start | End | Small String | Big String deriving (Eq, Ord)

instance Show Cave where
  show Start = "start"
  show End = "end"
  show (Small s) = s
  show (Big s) = s

type Edge = (Cave, Cave)

type Graph = M.Map Cave [Cave]

caveParser :: Parser Cave
caveParser =
  choice
    [ Start <$ "start",
      End <$ "end",
      Small <$> some (oneOf ['a' .. 'z']),
      Big <$> some (oneOf ['A' .. 'Z'])
    ]

edgeParser :: Parser Edge
edgeParser = (,) <$> (caveParser <* "-") <*> caveParser

graphParser :: Parser Graph
graphParser = foldr (\(a, b) -> M.insertWith (++) a [b] . M.insertWith (++) b [a]) M.empty <$> edgeParser `sepBy` "\n"

findPaths :: Graph -> Bool -> [[Cave]]
findPaths g = go Start (S.singleton Start)
  where
    go :: Cave -> S.Set Cave -> Bool -> [[Cave]]
    go cur seen allowDouble = let next = filter (isStepAllowed seen allowDouble) $ g M.! cur in combinePaths cur $ map (step seen allowDouble) next

    combinePaths :: Cave -> [[[Cave]]] -> [[Cave]]
    combinePaths el paths = concatMap (map (el :)) paths

    isStepAllowed seen allowDouble s = case s of
      Start -> False
      End -> True
      Big _ -> True
      Small _
        | s `S.notMember` seen -> True
        | allowDouble -> True
        | otherwise -> False

    step _ _ Start = error "not allowed"
    step _ _ End = [[End]]
    step seen allowDouble t@(Big _) = go t seen allowDouble
    step seen allowDouble t@(Small _)
      | t `S.notMember` seen = go t (S.insert t seen) allowDouble
      | allowDouble = go t (S.insert t seen) False
      | otherwise = []

part1 :: Graph -> Int
part1 = length . flip findPaths False

part2 :: Graph -> Int
part2 = length . flip findPaths True

main :: IO ()
main = do
  test1_1
  test1_2
  test1_3

  test2_1
  test2_2
  test2_3

  input <- parseInput 12 graphParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 12 graphParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1_1 :: IO ()
test1_1 = do
  input <- parseTest 12 1 graphParser
  guard $ part1 input == 10
  print ("ok" :: String)

test1_2 :: IO ()
test1_2 = do
  input <- parseTest 12 2 graphParser
  guard $ part1 input == 19
  print ("ok" :: String)

test1_3 :: IO ()
test1_3 = do
  input <- parseTest 12 3 graphParser
  guard $ part1 input == 226
  print ("ok" :: String)

test2_1 :: IO ()
test2_1 = do
  input <- parseTest 12 1 graphParser
  guard $ part2 input == 36
  print ("ok" :: String)

test2_2 :: IO ()
test2_2 = do
  input <- parseTest 12 2 graphParser
  guard $ part2 input == 103
  print ("ok" :: String)

test2_3 :: IO ()
test2_3 = do
  input <- parseTest 12 3 graphParser
  guard $ part2 input == 3509
  print ("ok" :: String)
