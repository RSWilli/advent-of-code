{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative
import Control.Monad
import InputParser
import Util

type Value a = Either String a

-- a datatype that holds wether or not the key has the correct data format
data Key
  = Byr (Value Int)
  | Iyr (Value Int)
  | Eyr (Value Int)
  | Hgt (Value (Int, String))
  | Hcl (Value String)
  | Ecl (Value String)
  | Pid (Value Int)
  | Other (Value String)
  deriving (Eq, Show)

type Passport = [Key]

parseAny :: Parser String
parseAny = some (satisfy (\x -> not (isSpace x || isControl x)))

validatingParser :: Parser a -> (a -> Bool) -> Parser (Value a)
validatingParser correctParser validator =
  try (correctParser >>= \res -> guard (validator res) >> return (Right res)) <|> Left <$> parseAny

validateHeight :: (Int, String) -> Bool
validateHeight (hgt, unit) = case unit of
  "cm" -> (150 <=> 193) hgt
  "in" -> (59 <=> 76) hgt

validateEyeColor :: String -> Bool
validateEyeColor ecl = elem ecl $ words "amb blu brn gry grn hzl oth"

numberParser = decimal <* notFollowedBy (satisfy isAlpha)

heightParser = (,) <$> decimal <*> ("in" <|> "cm")

hexParser = "#" *> some (satisfy isHexDigit)

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
      Other . Left <$> parseAny
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
isDataValid (Byr (Right _)) = True
isDataValid (Iyr (Right _)) = True
isDataValid (Eyr (Right _)) = True
isDataValid (Hgt (Right _)) = True
isDataValid (Hcl (Right _)) = True
isDataValid (Ecl (Right _)) = True
isDataValid (Pid (Right _)) = True
isDataValid _ = False

part1 :: [Passport] -> Int
part1 passes = length $ filter ((7 ==) . length . filter (not . isOther)) passes

-- currently bugged: 1 false positive
part2 :: [Passport] -> Int
part2 passes = length $ filter ((7 ==) . length . filter isDataValid) passes

main = do
  passports <- parseInput 4 passportsParser
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