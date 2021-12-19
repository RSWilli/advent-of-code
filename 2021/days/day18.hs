{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.String (IsString (fromString))
import InputParser
import Util (cartesianProduct)
import Prelude hiding (sum)

data SnailFishNumber = P SnailFishNumber SnailFishNumber | N Int deriving (Eq)

instance Show SnailFishNumber where
  show (N n) = show n
  show (P p1 p2) = "[" ++ show p1 ++ "," ++ show p2 ++ "]"

instance IsString SnailFishNumber where
  fromString str = mustParse snailFishNumberParser (fromString str)

instance Num SnailFishNumber where
  fromInteger = N . fromInteger
  x + y = reduce $ P x y -- note: this is not commutative
  (*) = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"
  negate = error "not implemented"

snailFishNumberParser :: Parser SnailFishNumber
snailFishNumberParser =
  choice
    [ P <$> ("[" *> snailFishNumberParser <* ",") <*> snailFishNumberParser <* "]",
      N <$> decimal
    ]

reduce :: SnailFishNumber -> SnailFishNumber
reduce = explodeAll
  where
    explodeAll x =
      let exploded = explode x
          splt = split exploded
       in if splt == x
            then splt
            else explodeAll splt

magnitude :: SnailFishNumber -> Int
magnitude (N n) = n
magnitude (P p1 p2) = 3 * magnitude p1 + 2 * magnitude p2

explode :: SnailFishNumber -> SnailFishNumber
explode s = let (_, x, _) = go 4 s in x
  where
    go :: Int -> SnailFishNumber -> (Int, SnailFishNumber, Int)
    go 0 (P (N p1) (N p2)) = (p1, N 0, p2)
    go n (P p1 p2) =
      let (ll, l, lr) = go (n - 1) p1
          r = addLeft lr p2
          (rl, r', rr) = go (n - 1) r
          l' = addRight rl l
       in (ll, P l' r', rr)
    go _ (N n') = (0, N n', 0)
    addLeft 0 x = x
    addLeft x (P p1 p2) = P (addLeft x p1) p2
    addLeft x (N n) = N (x + n)
    addRight 0 x = x
    addRight x (P p1 p2) = P p1 (addRight x p2)
    addRight x (N n) = N (x + n)

split :: SnailFishNumber -> SnailFishNumber
split = fst . split'
  where
    split' (N n)
      | n >= 10 = (P (N $ n `div` 2) (N $ (n + 1) `div` 2), True)
      | otherwise = (N n, False)
    split' (P n1 n2) =
      let (l, splitL) = split' n1
          (r, splitR) = split' n2
       in if splitL
            then (P l n2, splitL)
            else (P n1 r, splitR)

sum :: [SnailFishNumber] -> SnailFishNumber
sum = foldl1 (+) -- non commutative sum

part1 :: [SnailFishNumber] -> Int
part1 = magnitude . sum

part2 :: [SnailFishNumber] -> Int
part2 xs = maximum $ map (magnitude . uncurry (+)) $ filter (uncurry (/=)) $ cartesianProduct xs xs

main :: IO ()
main = do
  test11
  test12
  test13
  test14
  test15
  test16

  test2

  input <- parseInputLines 18 snailFishNumberParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 18 snailFishNumberParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test11 :: IO ()
test11 = do
  let e = explode "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
  guard $ e == "[[[[0,7],4],[15,[0,13]]],[1,1]]"
  print ("ok" :: String)

test12 :: IO ()
test12 = do
  let e = "[[[[4,3],4],4],[7,[[8,4],9]]]" + "[1,1]" :: SnailFishNumber
  guard $ e == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  print ("ok" :: String)

test13 :: IO ()
test13 = do
  input <- parseTestLines 18 1 snailFishNumberParser
  guard $ sum input == "[[[[1,1],[2,2]],[3,3]],[4,4]]"
  print ("ok" :: String)

test14 :: IO ()
test14 = do
  input <- parseTestLines 18 2 snailFishNumberParser
  guard $ sum input == "[[[[5,0],[7,4]],[5,5]],[6,6]]"
  print ("ok" :: String)

test15 :: IO ()
test15 = do
  input <- parseTestLines 18 3 snailFishNumberParser
  guard $ sum input == "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
  print ("ok" :: String)

test16 :: IO ()
test16 = do
  input <- parseTestLines 18 4 snailFishNumberParser
  guard $ part1 input == 4140
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTestLines 18 4 snailFishNumberParser
  guard $ part2 input == 3993
  print ("ok" :: String)