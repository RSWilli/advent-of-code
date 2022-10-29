{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import InputParser
import TwoD (Pos, print2D)
import Util (lIntListToInt)

type Algorithm = V.Vector Int

type Image = M.Map Pos Int

algorithmParser :: Parser Algorithm
algorithmParser = V.fromList . map mapChars <$> manyTill printChar "\n"

mapChars :: Num p => Char -> p
mapChars '.' = 0
mapChars '#' = 1
mapChars _ = error "Invalid character"

imageParser :: Parser Image
imageParser = toImage <$> many printChar `sepBy` "\n"
  where
    toImage = M.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x, y), mapChars c)) [0 ..]) [0 ..]

inputParser :: Parser (Algorithm, Image)
inputParser = (,) <$> (algorithmParser <* "\n") <*> imageParser

surrounding :: Pos -> [Pos]
surrounding (x, y) = [(x', y') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1]]

enhanceStep :: (Pos, Pos) -> Int -> Algorithm -> Image -> (Image, (Pos, Pos), Int)
enhanceStep ((minx, miny), (maxx, maxy)) def a i =
  let b@((minx', miny'), (maxx', maxy')) = ((minx - 1, miny - 1), (maxx + 1, maxy + 1))
   in ( M.fromList $ do
          x <- [minx' .. maxx']
          y <- [miny' .. maxy']
          let s = surrounding (x, y)
          let score = lIntListToInt 2 $ map (\p' -> M.findWithDefault def p' i) s
          let v = a V.! score
          return ((x, y), v),
        b,
        a V.! lIntListToInt 2 (replicate 9 def) -- in infinite space, the surrounding is always the default
      )

enhance :: Int -> Algorithm -> Image -> Image
enhance c a i0 = go 0 b0 c i0
  where
    b0 = (fst $ M.findMin i0, fst $ M.findMax i0)
    go _ _ 0 i = i
    go def b n i =
      let (i', b', def') = enhanceStep b def a i
       in go def' b' (n - 1) i'

printImage :: Image -> IO ()
printImage = print2D . mapMaybe (\(p, v) -> if v == 0 then Nothing else Just p) . M.toList

countActive :: Image -> Int
countActive = length . filter (== 1) . M.elems

part1 :: (Algorithm, Image) -> Int
part1 (a, i) = countActive $ enhance 2 a i

part2 :: (Algorithm, Image) -> Int
part2 (a, i) = countActive $ enhance 50 a i

main :: IO ()
main = do
  test1

  test2

  input <- parseInput 20 inputParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 20 inputParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 20 1 inputParser
  let i' = uncurry (enhance 2) input
  printImage i'
  print $ part1 input
  guard $ part1 input == 35
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 20 1 inputParser
  guard $ part2 input == 3351
  print ("ok" :: String)