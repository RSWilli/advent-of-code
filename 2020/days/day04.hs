{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import InputParser
import Util

data Key = Byr Bool | Iyr Bool | Eyr Bool | Hgt Bool | Hcl Bool | Ecl Bool | Pid Bool | Other Bool deriving (Eq, Show)

type Passport = [Key]

validKeys = S.fromList $ words "byr iyr eyr hgt hcl ecl pid" -- "cid"

isHex :: Char -> Bool
isHex c = isDigit c || ('a' <=> 'f') c

parseAny :: Parser String
parseAny = some (satisfy (\x -> not (isSpace x || isControl x)))

validatingParser :: Parser a -> (a -> Bool) -> Parser Bool
validatingParser correctParser validator =
  try (validator <$> correctParser) <|> False <$ parseAny

validateHeight :: (Int, String) -> Bool
validateHeight (hgt, unit) = case unit of
  "cm" -> (150 <=> 193) hgt
  "in" -> (59 <=> 76) hgt

validateEyeColor :: String -> Bool
validateEyeColor ecl = elem ecl $ words "amb blu brn gry grn hzl oth"

numberParser = decimal <* notFollowedBy (satisfy isAlpha)

heightParser = (,) <$> decimal <*> ("in" <|> "cm")

hexParser = "#" *> some (satisfy isHex)

keyParser :: Parser Key
keyParser =
  choice
    [ Byr <$> ("byr:" *> validatingParser numberParser (1920 <=> 2002)),
      Iyr <$> ("iyr:" *> validatingParser numberParser (2010 <=> 2020)),
      Eyr <$> ("eyr:" *> validatingParser numberParser (2020 <=> 2030)),
      Hgt <$> ("hgt:" *> validatingParser heightParser validateHeight),
      Hcl <$> ("hcl:" *> validatingParser hexParser ((6 ==) . length)),
      Ecl <$> ("ecl:" *> validatingParser name validateEyeColor),
      Pid <$> ("pid:" *> validatingParser numberParser (1000000000 >)),
      Other False <$ parseAny
    ]

passportParser :: Parser Passport
passportParser = sepEndBy keyParser (" " <|> "\n")

passportsParser :: Parser [Passport]
passportsParser = passportParser `sepBy` "\n"

isOther :: Key -> Bool
isOther key = case key of
  Other _ -> True
  _ -> False

isDataValid :: Key -> Bool
isDataValid (Byr x) = x
isDataValid (Iyr x) = x
isDataValid (Eyr x) = x
isDataValid (Hgt x) = x
isDataValid (Hcl x) = x
isDataValid (Ecl x) = x
isDataValid (Pid x) = x
isDataValid (Other _) = False

part1 :: [Passport] -> Int
part1 passes = length $ filter ((7 ==) . length . filter (not . isOther)) passes

part2 :: [Passport] -> Int
part2 passes = length $ filter ((7 ==) . length . filter isDataValid) passes

main = do
  passports <- parseInput 4 passportsParser
  print passports
  print $ part1 passports
  print $ part2 passports
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 4 passportsParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 passports,
          bench "part2" $ whnf part2 passports
        ]
    ]