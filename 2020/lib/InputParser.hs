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
import Data.Text (Text)
import Data.Void (Void)
import System.IO (readFile)
import Text.Megaparsec (Parsec, anySingle, choice, endBy, eof, many, manyTill, notFollowedBy, oneOf, parse, satisfy, sepBy, sepEndBy, some, someTill, try)
import Text.Megaparsec.Char (letterChar, newline, printChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

type Parser = Parsec Void String

myparse parser input = case parse (parser <* eof) "input" input of
  Left a -> Left (errorBundlePretty a)
  Right a -> Right a

getInputPath :: Int -> String
getInputPath = printf "inputs/day%02d.txt"

parseLines :: Parser a -> String -> Either String [a]
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

type Positions = M.Map Pos Char

getInput :: Int -> IO String
getInput i = readFile $ getInputPath i

parseInput2D :: Int -> IO Positions
parseInput2D i = do
  content <- lines <$> getInput i
  return $ M.fromList $ concat $ zipWith (\y line -> zipWith (\x char -> ((x, y), char)) [0 ..] line) [0 ..] content

dimensions :: Positions -> Pos
dimensions p = let ((width, height), _) = M.findMax p in (width + 1, height + 1)

parseTest :: String -> Parser a -> IO a
parseTest content parser = printParseError $ myparse parser content

parseTestLines :: String -> Parser a -> IO [a]
parseTestLines content parser = printParseError $ parseLines parser content

-- Number parser
number :: Integral a => Parser a
number = signed (return ()) decimal

-- name parser
name :: Parser String
name = some (satisfy isAlpha)

-- text parser match any text
text :: Parser String
text = some printChar