{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Sequence (Seq (Empty, (:<|)), fromList, (><))
import qualified Data.Set as S
import TwoD
  ( Pos,
    TwoD,
    dimensions,
    filter2D,
    inRange,
    insert2D,
    lookup2D,
    neighsDiag,
    parseInput2D,
    parseTest2D,
    updateAll2D,
  )

type Field = TwoD Int

step :: Field -> (Int, Field)
step field = go (shouldFlash incr) S.empty incr
  where
    incr = fmap (+ 1) field
    shouldFlash xs = fromList $ map fst $ filter2D ((> 9) . snd) xs
    go :: Seq Pos -> S.Set Pos -> Field -> (Int, Field)
    go Empty done f = (S.size done, f)
    go (first :<| q) done f =
      let done' = S.insert first done
          n = filter (`S.notMember` done') $ neighsDiag first
          f' = insert2D f first 0
          f'' = updateAll2D f' n (+ 1)
          todo' = q >< fromList [pos | pos <- n, inRange f'' pos, let Just v = lookup2D f'' pos, v == 10]
       in go todo' done' f''

run :: Int -> Field -> Int
run 0 _ = 0
run n field =
  let (score, field') = step field
   in score + run (n - 1) field'

findSync :: Field -> Int
findSync field = go 1 field
  where
    (height, width) = dimensions field
    size = height * width
    go n f =
      let (score, f') = step f
       in if score == size
            then n
            else go (n + 1) f'

part1 :: Field -> Int
part1 = run 100

part2 :: Field -> Int
part2 = findSync

main :: IO ()
main = do
  test1

  test2

  input <- parseInput2D 11 digitToInt

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput2D 11 digitToInt)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest2D 11 1 digitToInt
  guard $ part1 input == 1656
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest2D 11 1 digitToInt
  guard $ part2 input == 195
  print ("ok" :: String)
