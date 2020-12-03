module InputParser
  ( module InputParser,
    decimal,
    signed,
    space,
    letterChar,
  )
where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Void (Void)
import System.IO (readFile)
import Text.Megaparsec (Parsec, eof, parse, satisfy, sepBy, some)
import Text.Megaparsec.Char (letterChar, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed, space)
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
  content <- readFile $ getInputPath i
  printParseError $ myparse parser content

parseInputLines :: Int -> Parser a -> IO [a]
parseInputLines i parser = do
  content <- readFile $ getInputPath i
  printParseError $ parseLines parser content

type Pos = (Int, Int)

type Positions = M.Map Pos Char

parseInput2D :: Int -> IO Positions
parseInput2D i = do
  content <- lines <$> readFile (getInputPath i)
  return $ M.fromList $ concat $ zipWith (\y line -> zipWith (\x char -> ((x, y), char)) [0 ..] line) [0 ..] content

dimensions :: Positions -> Pos
dimensions p = let ((width, height), _) = M.findMax p in (width + 1, height + 1)

parseTest :: String -> Parser a -> IO a
parseTest path parser = do
  content <- readFile path
  printParseError $ myparse parser content

parseTestLines :: String -> Parser a -> IO [a]
parseTestLines path parser = do
  content <- readFile path
  printParseError $ parseLines parser content

-- Number parser
number :: Integral a => Parser a
number = signed (return ()) decimal

-- name parser
name :: Parser String
name = some (satisfy isAlpha)