{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import InputParser
import Util

type Passport = M.Map String String

validKeys =
  S.fromList
    [ "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
      -- "cid"
    ]

isValid :: S.Set String -> Bool
isValid = S.isSubsetOf validKeys

isHex :: Char -> Bool
isHex c = isDigit c || ('a' <=> 'f') c

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
isValidData :: Passport -> Bool
isValidData pass = isJust $
  do
    byr <- read <$> M.lookup "byr" pass
    guard $ (1920 <=> 2002) byr

    iyr <- read <$> M.lookup "iyr" pass
    guard $ (2010 <=> 2020) iyr

    eyr <- read <$> M.lookup "eyr" pass
    guard $ (2020 <=> 2030) eyr

    (hgt, unit) <- span isDigit <$> M.lookup "hgt" pass
    guard $ case unit of
      "cm" -> (150 <=> 193) (read hgt)
      "in" -> (59 <=> 76) (read hgt)
      _ -> False

    '#' : hcl <- M.lookup "hcl" pass
    guard $ all isHex hcl
    guard $ length hcl == 6

    ecl <- M.lookup "ecl" pass
    guard $ ecl `elem` words "amb blu brn gry grn hzl oth"

    pid <- M.lookup "pid" pass
    guard $ all isDigit pid
    guard $ length pid == 9

keyValueParser :: Parser (String, String)
keyValueParser = (,) <$> manyTill (satisfy isAlpha) ":" <*> many (satisfy (not . isSpace))

passportParser :: Parser Passport
passportParser = M.fromList <$> sepEndBy keyValueParser (" " <|> "\n")

passportsParser :: Parser [Passport]
passportsParser = passportParser `sepBy` "\n"

validPassportsP1 :: [Passport] -> [Passport]
validPassportsP1 = filter (isValid . M.keysSet)

validPassportsP2 :: [Passport] -> [Passport]
validPassportsP2 passes = filter isValidData $ filter (isValid . M.keysSet) passes

part1 :: [Passport] -> Int
part1 passes = length $ validPassportsP1 passes

part2 :: [Passport] -> Int
part2 passes = length $ validPassportsP2 passes

main = do
  passports <- parseInput 4 passportsParser
  print $ part1 passports
  print $ part2 passports

-- defaultMain
--   [ bgroup
--       "run"
--       [ bench "part1" $ whnf part1 mountain,
--         bench "part2" $ whnf part2 mountain
--       ]
--   ]