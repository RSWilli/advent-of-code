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
    Parsec,
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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlpha, isControl, isDigit, isHexDigit, isSpace)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, endBy, eof, many, manyTill, notFollowedBy, oneOf, parse, satisfy, sepBy, sepEndBy, some, someTill, try, (<?>))
import Text.Megaparsec.Char (hspace, hspace1, letterChar, newline, printChar, space, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, signed)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Stream (TraversableStream, VisualStream)
import Text.Printf (printf)
import Util

type Parser = Parsec Void Text

myparse :: (TraversableStream a, VisualStream a) => Parsec Void a b -> a -> Either String b
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

type Positions = V.Vector (V.Vector Char)

getInput :: Int -> IO Text
getInput i = T.readFile $ getInputPath i

getTest :: Int -> Int -> IO Text
getTest i j = T.readFile $ getTestPath i j

parseInput2D :: Int -> IO Positions
parseInput2D i = do
  content <- BS.lines <$> BS.readFile (getInputPath i)
  return $ V.fromList $ map (V.fromList . BS.unpack) content

parseTest2D :: Int -> Int -> IO Positions
parseTest2D i j = do
  content <- BS.lines <$> BS.readFile (getTestPath i j)
  return $ V.fromList $ map (V.fromList . BS.unpack) content

lookup2D :: Positions -> Pos -> Char
lookup2D poses (x, y) = (poses V.! y) V.! x

map2D :: (Pos -> Char -> Char) -> Positions -> Positions
map2D fn = V.imap (\y -> V.imap (\x c -> fn (x, y) c))

show2D :: Positions -> String
show2D poses = unlines $ V.toList $ V.map V.toList poses

print2D :: Positions -> IO ()
print2D poses = putStrLn $ show2D poses

dimensions :: Positions -> Pos
dimensions p =
  let height = V.length p
      width = V.length (p V.! 0)
   in (width, height)

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

doubleTicks :: Parser a -> Parser a
doubleTicks = between (symbol "\"") (symbol "\"")