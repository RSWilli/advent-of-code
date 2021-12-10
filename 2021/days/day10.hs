{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Bench
import Control.Monad (guard)
import qualified Data.Text as T
import InputParser
import Util (lIntListToInt, median)

data LineType = Incomplete String | Correct | Incorrect Char Char
  deriving (Eq, Show)

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing _ = error "Invalid closing"

getWrongBracket :: T.Text -> LineType
getWrongBracket t = go [] $ T.unpack t
  where
    go :: String -> String -> LineType
    go [] "" = Correct
    go stack "" = Incomplete stack
    go (h : stack) (c : str) = case c of
      '[' -> go (']' : h : stack) str
      '(' -> go (')' : h : stack) str
      '{' -> go ('}' : h : stack) str
      '<' -> go ('>' : h : stack) str
      _ ->
        if h == c
          then go stack str
          else Incorrect c h
    go [] (x : xs) = go [closing x] xs

isIncomplete :: LineType -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

wrongBracketScore :: LineType -> Int
wrongBracketScore Correct = 0
wrongBracketScore (Incorrect ')' _) = 3
wrongBracketScore (Incorrect ']' _) = 57
wrongBracketScore (Incorrect '}' _) = 1197
wrongBracketScore (Incorrect '>' _) = 25137
wrongBracketScore (Incorrect _ _) = error "Invalid bracket"
wrongBracketScore (Incomplete _) = 0

getIncompleteScores :: LineType -> Int
getIncompleteScores (Incomplete xs) = lIntListToInt 5 $ map getscore xs
  where
    getscore :: Char -> Int
    getscore ')' = 1
    getscore ']' = 2
    getscore '}' = 3
    getscore '>' = 4
    getscore _ = error "Invalid bracket"
getIncompleteScores _ = error "not icomplete"

part1 :: [T.Text] -> Int
part1 = sum . map (wrongBracketScore . getWrongBracket)

part2 :: [T.Text] -> Int
part2 = round . median . map getIncompleteScores . filter isIncomplete . map getWrongBracket

main :: IO ()
main = do
  test1

  test2

  input <- T.lines <$> getInput 10

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (T.lines <$> getInput 10)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- T.lines <$> getTest 10 1
  guard $ part1 input == 26397
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- T.lines <$> getTest 10 1
  guard $ part2 input == 288957
  print ("ok" :: String)