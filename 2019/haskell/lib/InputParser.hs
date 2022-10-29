{-# Language OverloadedStrings #-}
module InputParser 
( module InputParser
, satisfy, anySingle, sepBy, manyTill, space, string, many, some, parseTest
) where

import Text.Megaparsec (setInput, anySingle, satisfy, parse, Parsec, eof, sepBy, manyTill, parseTest)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed, space)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Char (isAlpha)
import Control.Applicative (some, many)
import Data.Void
import Text.Printf
import System.Environment (getArgs)
import System.IO (readFile, getContents)

getFilename :: Int -> String
getFilename = printf "inputs/day%02d.txt"

-- open the input file for day i
getRawInput :: Int -> IO String
getRawInput i = do
  args <- getArgs
  case args of
    [] -> readFile $ getFilename i
    "-":_ -> getContents
    filename:_ -> readFile filename


-- Parsing of input


type Parser = Parsec Void String

-- parse input line by line
parseLines :: Parser a -> String -> Either String [a]
parseLines p input =
  case parse (traverse parse1 (lines input)) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a
  where
    parse1 x = setInput x *> p <* eof <* setInput "\n" <* newline

-- Run a parser with 'parseLines' on the input file.
getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines i p =
  do input <- getRawInput i
     either fail return (parseLines p input)

getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p = do 
  input <- getRawInput i
  case parse p "input" input of
    Left e -> fail (errorBundlePretty e)
    Right a -> return a


-- Number parser
number :: Integral a => Parser a
number = signed (return ()) decimal

-- name parser
name :: Parser String
name = some (satisfy isAlpha)