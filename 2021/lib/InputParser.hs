{-# LANGUAGE OverloadedStrings #-}

module InputParser
  ( module InputParser,
    decimal,
    signed,
    space,
    letterChar,
    printChar,
    sepBy,
    (<?>),
    satisfy,
    isSpace,
    isAlpha,
    endBy,
    newline,
    manyTill,
    sepEndBy,
    isDigit,
    eof,
    choice,
    someTill,
    isControl,
    try,
    anySingle,
    notFollowedBy,
    hspace,
    isHexDigit,
    binary,
    oneOf,
    many,
  )
where

import Control.Applicative (empty)
import Data.Char (isAlpha, isControl, isDigit, isHexDigit, isSpace)
import Data.Either (fromRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle (bundleErrors), Parsec, anySingle, between, choice, endBy, eof, many, manyTill, notFollowedBy, oneOf, parse, satisfy, sepBy, sepEndBy, some, someTill, try, (<?>))
import Text.Megaparsec.Char (hspace, hspace1, letterChar, newline, printChar, space, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, signed)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.Megaparsec.Error as PE
import Text.Megaparsec.Stream (Stream, TraversableStream, VisualStream)
import Text.Printf (printf)
import Util

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

tryParse :: Parser a -> Text -> Either ParseError a
tryParse parser = parse (parser <* eof) "input"

-- generalized parser that can parse any stream, but fails if the parser does
mustParse :: Parser a -> Text -> a
mustParse p i = either (error . errorBundlePretty) id $ tryParse p i

getInputPath :: Int -> String
getInputPath = printf "inputs/day%02d.txt"

getTestPath :: Int -> Int -> String
getTestPath = printf "tests/day%02d_%d.txt"

parseLines :: Parser a -> Text -> Either ParseError [a]
parseLines parser = tryParse combinedParser
  where
    combinedParser = parser `sepBy` newline

printParseError :: Either ParseError a -> IO a
printParseError = either (fail . errorBundlePretty) return

parseInput :: Int -> Parser a -> IO a
parseInput i parser = do
  content <- getInput i
  printParseError $ tryParse parser content

parseInputLines :: Int -> Parser a -> IO [a]
parseInputLines i parser = do
  content <- getInput i
  printParseError $ parseLines parser content

tryParseInputLines :: Int -> Parser a -> IO [Either ParseError a]
tryParseInputLines i parser = do
  content <- T.lines <$> getInput i
  return $ map (tryParse parser) content

getInput :: Int -> IO Text
getInput i = T.readFile $ getInputPath i

getTest :: Int -> Int -> IO Text
getTest i j = T.readFile $ getTestPath i j

parseTest :: Int -> Int -> Parser a -> IO a
parseTest i j parser = do
  content <- getTest i j
  printParseError $ tryParse parser content

parseTestLines :: Int -> Int -> Parser a -> IO [a]
parseTestLines i j parser = do
  content <- getTest i j
  printParseError $ parseLines parser content

tryParseTestLines :: Int -> Int -> Parser a -> IO [Either ParseError a]
tryParseTestLines i j parser = do
  content <- T.lines <$> getTest i j
  return $ map (tryParse parser) content

-- Number parser
number :: Parser Int
number = signed (return ()) decimal

-- text parser match any text
text :: Parser String
text = lexeme $ some $ satisfy isAlpha

spaceConsumer = L.space hspace1 empty empty

symbol = L.symbol spaceConsumer

lexeme = L.lexeme spaceConsumer

integer :: (Num a) => Parser a
integer = lexeme decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

doubleTicks :: Parser a -> Parser a
doubleTicks = between (symbol "\"") (symbol "\"")

-- TODO:
-- getExpectedTokens :: ParseError -> [Char]
-- getExpectedTokens e = let ex = bundleErrors e in foldl (\acc e -> getToken e ++ acc) [] ex
--   where
--     getToken :: PE.ParseError Text Void -> [Char]
--     getToken (PE.TrivialError _ (Just (PE.Tokens i)) expected) =  -- NE.toList i
--     getToken _ = []