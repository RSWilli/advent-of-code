{-# LANGUAGE OverloadedStrings #-}

import Bench
import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard)
import InputParser
import TwoD (Pos)
import Util (countUnique, gaussianSum, invertGauss)

targetAreaParser :: Parser (Pos, Pos)
targetAreaParser = toBounds <$> ("target area: x=" *> number <* "..") <*> (number <* ", y=") <*> (number <* "..") <*> number
  where
    toBounds x1 x2 y1 y2 = ((x1, y1), (x2, y2))

-- x :: Int -> Int -> Int
-- x vx t =
--   if t >= vx
--     then gaussianSum vx
--     else gaussianSum vx - gaussianSum (vx - t)

-- y :: Int -> Int -> Int
-- y vy t = vy * t - gaussianSum (t - 1)

invertX :: Int -> Int -> Maybe Double
invertX ivx ixdist =
  let vx = fromIntegral ivx :: Double
      xdist = fromIntegral ixdist :: Double
      diskr = (2 * vx + 1) * (2 * vx + 1) - 8 * xdist
   in -- else Just $ 1 / 2 * (1 + sqrt diskr + 2 * vx) -- the positive root is the second time the gaussian sum reaches the xdistance, since the second part will overtake the first
      if diskr < 0
        then Nothing
        else Just $ 1 / 2 * (1 - sqrt diskr + 2 * vx)

invertY :: Int -> Int -> Maybe (Double, Double)
invertY ivy iydist =
  let vy = fromIntegral ivy :: Double
      ydist = fromIntegral iydist :: Double
      diskr = (2 * vy + 1) * (2 * vy + 1) - 8 * ydist
   in if diskr < 0
        then Nothing
        else
          Just
            ( 0.5 * (sqrt diskr + 2 * vy + 1), -- the first time the ydistance is reached
              - 0.5 * sqrt diskr + vy + 0.5 -- the second time the ydistance is reached
            )

xstepInBounds :: Int -> Int -> Int -> Maybe (Int, Maybe Int)
xstepInBounds vx xmin xmax = do
  tmin <- ceiling <$> invertX vx xmin
  let tmax = floor <$> invertX vx xmax
  case tmax of
    Nothing -> return (tmin, Nothing)
    Just tmax' -> do
      guard (tmin <= tmax')
      return (tmin, tmax)

ystepInBounds :: Int -> Int -> Int -> Maybe (Int, Maybe Int)
ystepInBounds vy ymin ymax = do
  tmin <- ceiling . fst <$> invertY vy ymax
  let tmax = floor . fst <$> invertY vy ymin
  case tmax of
    Nothing -> return (tmin, Nothing)
    Just tmax' -> do
      guard (tmin <= tmax')
      return (tmin, tmax)

boundsToRange :: Int -> Maybe (Int, Maybe Int) -> [Int]
boundsToRange _ Nothing = []
boundsToRange m (Just (lo, hi)) = maybe [lo .. m] (enumFromTo lo) hi

throwAll :: (Pos, Pos) -> Int
throwAll ((xmin, ymin), (xmax, ymax)) =
  let vxmin = invertGauss xmin
      Just tmax = ceiling . fst <$> invertY (- ymin - 1) ymin -- the moment where the maxmimum y force reaches the bottom of the box
      vx = do
        v <- [vxmin .. xmax]
        t <- boundsToRange tmax $ xstepInBounds v xmin xmax
        return (v, t)

      vy = do
        v <- [ymin .. - ymin - 1]
        t <- boundsToRange tmax $ ystepInBounds v ymin ymax
        return (t, v)
   in countUnique [(vx0, vy0) | ((vx0, t), (t', vy0)) <- liftA2 (,) vx vy, t == t']

part1 :: (Pos, Pos) -> Int
part1 ((_, ymin), _) = gaussianSum $ - ymin - 1

part2 :: (Pos, Pos) -> Int
part2 = throwAll

main :: IO ()
main = do
  test1
  test2

  input <- parseInput 17 targetAreaParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 17 targetAreaParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test1 :: IO ()
test1 = do
  input <- parseTest 17 1 targetAreaParser
  guard $ part1 input == 45
  print ("ok" :: String)

test2 :: IO ()
test2 = do
  input <- parseTest 17 1 targetAreaParser
  print $ part2 input
  guard $ part2 input == 112
  print ("ok" :: String)