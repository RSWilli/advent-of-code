module InputParser
  ( module InputParser,
    parse,
  )
where

import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed, space)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf

type Parser = Parsec Void String

myparse parser input = case parse parser "input" input of
  Left a -> Left (errorBundlePretty a)
  Right a -> Right a

getInputPath :: Int -> String
getInputPath = printf "inputs/day%02d.txt"

parseLines :: Parser a -> String -> Either String [a]
parseLines parser = myparse combinedParser
  where
    combinedParser = (parser `sepBy` newline) <* eof

printParseError :: Either String a -> IO a
printParseError = either fail return

parseInput :: Int -> Parser a -> IO a
parseInput i parser = do
  content <- readFile $ getInputPath i
  printParseError $ myparse parser content

parseInputLines :: Int -> Parser a -> IO [a]
parseInputLines i parser = do
  content <- readFile $ getInputPath i
  printParseError $ parseLines parser content

-- Number parser
number :: Integral a => Parser a
number = signed (return ()) decimal

-- name parser
name :: Parser String
name = some (satisfy isAlpha)