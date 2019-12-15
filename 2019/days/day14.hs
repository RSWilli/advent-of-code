{-# Language OverloadedStrings #-}
import InputParser
import Data.List (intercalate)
import qualified Data.Map as M

data Chemical = Chemical Int String deriving (Ord, Eq)
instance Show Chemical where
  show (Chemical amount n) = show amount ++ " " ++ n

data Reaction = Reaction [Chemical] Chemical
instance Show Reaction where
  show (Reaction input output) = intercalate ", " (map show input) ++ " => " ++ show output

chemicalParser :: Parser Chemical
chemicalParser = Chemical <$> (number <* " ") <*> name

chemicalListParser :: Parser [Chemical]
chemicalListParser = chemicalParser `sepBy` ", "

reactionParser :: Parser Reaction
reactionParser = Reaction <$> (chemicalListParser <* " => ") <*> chemicalParser

main :: IO ()
main = do
  reactions <- getParsedLines 14 reactionParser
  let reactionTree = M.fromList $ map (\(Reaction s (Chemical c n)) -> (n, (c,s))) reactions
  print $ M.lookup "FUEL" reactionTree
  