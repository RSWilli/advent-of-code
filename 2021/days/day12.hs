{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative (Alternative (some))
import Control.Monad (guard)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import GHC.Generics (Generic)
import InputParser

data Cave = Start | End | Small String | Big String deriving (Eq, Ord, Generic)

--https://hackage.haskell.org/package/hashable-1.4.0.0/docs/Data-Hashable.html#g:4
instance Hashable Cave

instance Show Cave where
  show Start = "start"
  show End = "end"
  show (Small s) = s
  show (Big s) = s

type Edge = (Cave, Cave)

type Graph = M.HashMap Cave [Cave]

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
graphParser = foldr (\(a, b) -> M.insertWith (++) a [b] . M.insertWith (++) b [a | a /= Start]) M.empty <$> edgeParser `sepBy` "\n"

countPaths :: Graph -> Bool -> Int
countPaths g = expand Start (S.singleton Start)
  where
    expand :: Cave -> S.HashSet Cave -> Bool -> Int
    expand cur seen allowDouble = let next = g M.! cur in sum $ map (step seen allowDouble) next

    step _ _ Start = 0
    step _ _ End = 1
    step seen allowDouble t@(Big _) = expand t seen allowDouble
    step seen allowDouble t@(Small _)
      | not $ t `S.member` seen = expand t (S.insert t seen) allowDouble
      | allowDouble = expand t (S.insert t seen) False
      | otherwise = 0

part1 :: Graph -> Int
part1 = flip countPaths False

part2 :: Graph -> Int
part2 = flip countPaths True

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
