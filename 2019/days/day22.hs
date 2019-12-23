{-# Language OverloadedStrings #-}
import InputParser
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Applicative

len :: Int
len = 10007

type Deck = V.Vector Int

newDeck :: Int -> Deck
newDeck l = V.generate l id

data ShuffleInstruction = Deal
                        | DealWithIncreament Int
                        | Cut Int
                        deriving Show

dealParser :: Parser ShuffleInstruction
dealParser = Deal <$ string "deal into new stack"

dealWithIncrementParser :: Parser ShuffleInstruction
dealWithIncrementParser = DealWithIncreament <$> ( string "deal with increment " *> number)

cutParser :: Parser ShuffleInstruction
cutParser = Cut <$> ( string "cut " *> number)

instructionParser :: Parser ShuffleInstruction
instructionParser = dealParser <|> dealWithIncrementParser <|> cutParser

deck1 :: Deck
deck1 = newDeck len

cut :: Int -> Deck -> Deck
cut i d = let splitPos = if i >= 0 then i else V.length d + i
              (inits, tails) = V.splitAt splitPos d 
          in tails V.++ inits

deal :: Deck -> Deck
deal = V.reverse

dealWithIncrement :: Int -> Deck -> Deck
dealWithIncrement i d = let l = V.length d
                            start = (0, M.empty)
                            dealToTable (pos, m) el = ((pos + i) `mod` l, M.insert pos el m)
                            table = snd $ V.foldl dealToTable start d
                        in V.fromList $ map snd $ M.toList table

shuffle :: Deck -> ShuffleInstruction -> Deck
shuffle d ins = case ins of
  Deal -> deal d
  DealWithIncreament i -> dealWithIncrement i d
  Cut i -> cut i d

main :: IO()
main = do
  ins <- V.fromList <$> getParsedLines 22 instructionParser
  let finalDeck = foldl shuffle deck1 ins

  putStrLn "Part1:"
  print $ V.findIndex (2019 == ) finalDeck