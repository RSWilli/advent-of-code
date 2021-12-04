{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Bench
import Control.Applicative hiding (many)
import Control.Monad (guard)
import qualified Data.IntMap as M
import Data.List (partition)
import InputParser

data Bingo = Bingo
  { numberPoses :: M.IntMap Pos,
    rowScores :: M.IntMap Int,
    colScores :: M.IntMap Int
  }
  deriving (Show)

drawListParser :: Parser [Int]
drawListParser = number `sepBy` ","

bingoParser :: Parser Bingo
bingoParser = do
  nums <- some $ rowParser <* (() <$ "\n" <|> eof)
  let numPoses = concat $ zipWith (\ls y -> zipWith (\l x -> (l, (x, y))) ls [0 ..]) nums [0 ..]
  return $ Bingo (M.fromList numPoses) M.empty M.empty
  where
    rowParser = some (hspace *> number)

inputParser :: Parser ([Int], [Bingo])
inputParser = do
  drawList <- drawListParser <* "\n\n"
  bingoList <- bingoParser `sepBy` "\n"
  return (drawList, bingoList)

drawNumber :: Int -> Bingo -> Bingo
drawNumber n bingo =
  let pos = numberPoses bingo M.!? n
   in case pos of
        Just (x, y) ->
          bingo
            { numberPoses = M.delete n (numberPoses bingo),
              rowScores = M.insertWith (+) x 1 (rowScores bingo),
              colScores = M.insertWith (+) y 1 (colScores bingo)
            }
        Nothing -> bingo

finished :: Bingo -> Bool
finished bingo = any ((== 5) . snd) (M.toList (colScores bingo) ++ M.toList (rowScores bingo))

playBingo :: [Int] -> [Bingo] -> [Int]
playBingo [] _ = []
playBingo (x : xs) bingos =
  let newBingos = map (drawNumber x) bingos
      (winners, losers) = partition finished newBingos
   in map (bingoScore x) winners ++ playBingo xs losers

bingoScore :: Int -> Bingo -> Int
bingoScore n bingo = n * M.foldrWithKey (\k _ s -> k + s) 0 (numberPoses bingo)

part1 :: ([Int], [Bingo]) -> Int
part1 (xs, bingos) = head $ playBingo xs bingos

part2 :: ([Int], [Bingo]) -> Int
part2 (xs, bingos) = last $ playBingo xs bingos

main :: IO ()
main = do
  test1
  test2

  input <- parseInput 4 inputParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 4 inputParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 4 1 inputParser
  guard $ part1 input == 4512
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 4 1 inputParser
  guard $ part2 input == 1924
  print ("ok" :: [Char])
