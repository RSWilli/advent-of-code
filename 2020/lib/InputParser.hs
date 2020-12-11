{-# LANGUAGE OverloadedStrings #-}

module InputParser
  ( module InputParser,
    decimal,
    signed,
    space,
    letterChar,
    printChar,
    sepBy,
    satisfy,
    isSpace,
    isAlpha,
    endBy,
    manyTill,
    sepEndBy,
    isDigit,
    choice,
    someTill,
    isControl,
    try,
    anySingle,
    notFollowedBy,
    isHexDigit,
    oneOf,
  )
where

import Data.Char (isAlpha, isControl, isDigit, isHexDigit, isSpace)
import qualified Data.Map as M
import Data.Text (Text, lines, unpack)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, endBy, eof, many, manyTill, notFollowedBy, oneOf, parse, satisfy, sepBy, sepEndBy, some, someTill, try)
import Text.Megaparsec.Char (letterChar, newline, printChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)
import Util
import Prelude hiding (lines, readFile)

type Parser = Parsec Void Text

myparse parser input = case parse (parser <* eof) "input" input of
  Left a -> Left (errorBundlePretty a)
  Right a -> Right a

getInputPath :: Int -> String
getInputPath = printf "inputs/day%02d.txt"

getTestPath :: Int -> Int -> String
getTestPath = printf "tests/day%02d_%d.txt"

parseLines :: Parser a -> Text -> Either String [a]
parseLines parser = myparse combinedParser
  where
    combinedParser = parser `sepBy` newline

printParseError :: Either String a -> IO a
printParseError = either fail return

parseInput :: Int -> Parser a -> IO a
parseInput i parser = do
  content <- getInput i
  printParseError $ myparse parser content

parseInputLines :: Int -> Parser a -> IO [a]
parseInputLines i parser = do
  content <- getInput i
  printParseError $ parseLines parser content

type Pos = (Int, Int)

type TwoDimensional = M.Map Pos

type Positions = M.Map Pos Char

getInput :: Int -> IO Text
getInput i = readFile $ getInputPath i

getTest :: Int -> Int -> IO Text
getTest i j = readFile $ getTestPath i j

parseInput2D :: Int -> IO Positions
parseInput2D i = do
  content <- lines <$> getInput i
  return $ parse2D content

parseTest2D :: Int -> Int -> IO Positions
parseTest2D i j = do
  content <- lines <$> getTest i j
  return $ parse2D content

parse2D :: [Text] -> Positions
parse2D content =
  M.fromList $
    reverse $
      concat $
        zipWith
          ( \y line ->
              zipWith
                ( \x char ->
                    ((x, y), char)
                )
                [0 ..]
                line
          )
          [0 ..]
          (map unpack content)

print2D :: Positions -> IO ()
print2D pos =
  let (width, height) = dimensions pos
      poses = do
        y <- [0 .. (height -1)]
        x <- [0 .. (width -1)]
        return $ pos M.! (x, y)
   in putStrLn $ unlines $ chunks width poses

dimensions :: Positions -> Pos
dimensions p = let ((width, height), _) = M.findMax p in (width + 1, height + 1)

parseTest :: Int -> Int -> Parser a -> IO a
parseTest i j parser = do
  content <- getTest i j
  printParseError $ myparse parser content

parseTestLines :: Int -> Int -> Parser a -> IO [a]
parseTestLines i j parser = do
  content <- getTest i j
  printParseError $ parseLines parser content

-- Number parser
number :: Parser Int
number = signed (return ()) decimal

-- name parser
name :: Parser String
name = some (satisfy isAlpha)

-- text parser match any text
text :: Parser String
text = some printChar