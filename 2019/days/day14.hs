{-# Language OverloadedStrings #-}
import InputParser
import qualified Data.Map as M

type Reactions = M.Map String (Int, M.Map String Int)

type Chemical = (String, Int)

chemicalParser :: Parser Chemical
chemicalParser = (flip (,)) <$> (number <* " ") <*> name

chemicalListParser :: Parser [Chemical]
chemicalListParser = chemicalParser `sepBy` ", "

reactionParser :: Parser (String, (Int, M.Map String Int))
reactionParser = do
  srcChemList <- chemicalListParser
  " => "
  (name, count) <- chemicalParser

  return (name, (count, M.fromList srcChemList))

-- create

main :: IO ()
main = do
  reactions <- M.fromList <$> getParsedLines 14 reactionParser
  print $ M.lookup "FUEL" reactions
  