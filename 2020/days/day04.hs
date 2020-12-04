{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import InputParser
import Util

type Passport = M.Map String String

validKeys = S.fromList $ words "byr iyr eyr hgt hcl ecl pid" -- "cid"

isHex :: Char -> Bool
isHex c = isDigit c || ('a' <=> 'f') c

isValueInRange :: Int -> Int -> String -> Passport -> Maybe ()
isValueInRange min max key pass = M.lookup key pass >>= guard . (min <=> max) . read

isValidData :: Passport -> Bool
isValidData pass = isJust $
  do
    isValueInRange 1920 2002 "byr" pass
    isValueInRange 2010 2020 "iyr" pass
    isValueInRange 2020 2030 "eyr" pass

    (hgt, unit) <- span isDigit <$> M.lookup "hgt" pass
    guard $ case unit of
      "cm" -> (150 <=> 193) (read hgt)
      "in" -> (59 <=> 76) (read hgt)
      _ -> False

    '#' : hcl <- M.lookup "hcl" pass
    guard $ all isHex hcl && length hcl == 6

    ecl <- M.lookup "ecl" pass
    guard $ ecl `elem` words "amb blu brn gry grn hzl oth"

    pid <- M.lookup "pid" pass
    guard $ all isDigit pid && length pid == 9

keyValueParser :: Parser (String, String)
keyValueParser = (,) <$> manyTill (satisfy isAlpha) ":" <*> many (satisfy (not . isSpace))

passportParser :: Parser Passport
passportParser = M.fromList <$> sepEndBy keyValueParser (" " <|> "\n")

passportsParser :: Parser [Passport]
passportsParser = passportParser `sepBy` "\n"

part1 :: [Passport] -> Int
part1 passes = length $ filter (S.isSubsetOf validKeys . M.keysSet) passes

part2 :: [Passport] -> Int
part2 passes = length $ filter isValidData passes

main = do
  passports <- parseInput 4 passportsParser
  print $ part1 passports
  print $ part2 passports
  defaultMain
    [ bgroup
        "run"
        [ bench "part1" $ whnf part1 passports,
          bench "part2" $ whnf part2 passports
        ]
    ]