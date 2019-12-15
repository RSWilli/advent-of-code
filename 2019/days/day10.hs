{-# Language OverloadedStrings #-}
import Data.Function (fix)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import InputParser
import Util
import Control.Applicative (many, (<|>))

data AsteroidField = AsteroidField {
  width :: Int, 
  height :: Int, 
  field :: S.Set (Int, Int)
} deriving (Show)

parse :: IO AsteroidField
parse = do
  input <- getParsedLines 10 (many $ True <$ "#" <|> False <$ ".")
  let astf = S.fromList [(x,y) | (y, row) <- index input, (x, True) <- index row]
  let w = length $ head input
  let h = length input
  return $ AsteroidField {width = w, height = h, field = astf}

shadow :: (Int, Int) -> (Int, Int) -> AsteroidField -> AsteroidField
shadow (startX, startY) (astX, astY) asteroidfield =
  let w = width asteroidfield
      h = height asteroidfield

      diffX = astX - startX
      diffY = astY - startY

      g = gcd diffX diffY

      deltaX = diffX `div` g
      deltaY = diffY `div` g

      shadowedPositions = getPositions deltaX deltaY w h astX astY

      newastfield = foldl (flip S.delete) (field asteroidfield) shadowedPositions
      in
        asteroidfield {field = newastfield}



getPositions :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getPositions stepX stepY widthX widthY = 
    fix (\g posX posY ->
        let newPosX = posX + stepX
            newPosY = posY + stepY
            in
              if newPosX < widthX && newPosX >= 0 && newPosY < widthY && newPosY >= 0
                then (newPosX, newPosY) : g newPosX newPosY
                else []
      )

visibleFrom :: (Int, Int) -> AsteroidField -> Int
visibleFrom start asteroidfield = let shadowAsteroid = shadow start
                                      astf = asteroidfield { field = S.delete start $ field asteroidfield }

                                      newastfield = S.foldl 
                                        (flip shadowAsteroid) 
                                        astf 
                                        $ field astf
                                      in
                                        S.size $ field newastfield


getAllViews :: AsteroidField -> [(Int, Int, Int)]
getAllViews astf = 
  let asteroids = field astf
      in
        foldl (\list ast@(x,y) -> (visibleFrom ast astf, x,y):list) [] asteroids

-- get the angle -> adjusted to be relative to the y axis 
getAngle :: Int -> Int -> Int -> Int -> Double
getAngle sx sy x y = let ang = pi/2 + atan2 (fromIntegral $ y-sy) (fromIntegral $ x-sx)
                         in
                           if ang > 2*pi then ang - 2*pi else if ang < 0 then ang + 2* pi else ang


getDistance :: (Floating b, Integral a) => a -> a -> a -> a -> b
getDistance x1 y1 x2 y2 = let nx = fromIntegral $ x2 - x1 
                              ny = fromIntegral $ y2 - y1
                              in
                                abs $ sqrt $ nx^2 + ny^2

orderAngles :: Int -> Int -> (Int, Int) -> (Int, Int) -> Ordering
orderAngles startX startY (x1,y1) (x2,y2) = let ang1 = getAngle startX startY x1 y1
                                                ang2 = getAngle startX startY x2 y2

                                                in
                                                  compare ang1 ang2

angleEqual :: Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
angleEqual startX startY (x1,y1) (x2,y2) = let ang1 = getAngle startX startY x1 y1
                                               ang2 = getAngle startX startY x2 y2

                                               in
                                                 ang1 == ang2

orderDistance :: Int -> Int -> (Int, Int) -> (Int, Int) -> Ordering
orderDistance startX startY (x1,y1) (x2,y2) = let d1 = getDistance startX startY x1 y1
                                                  d2 = getDistance startX startY x2 y2

                                                  in
                                                    compare d1 d2


getLaserOrder :: [[(Int, Int)]] -> [(Int, Int)]
getLaserOrder [] = []
getLaserOrder (x:xs) = case x of
  [] -> getLaserOrder xs
  y:ys -> y : getLaserOrder(xs ++ [ys])

main :: IO()
main = do
  astf <- parse
  let (bestVis, bestX, bestY) = maximum $ getAllViews astf
  print bestVis

  let orderFn = orderAngles bestX bestY
  let eqFn = angleEqual bestX bestY

  let orderDistFn = orderDistance bestX bestY

  let orderedAngles = map (sortBy orderDistFn) $ groupBy eqFn $ sortBy orderFn $ S.toList $ field astf

  let lasered = M.fromList $ zip [1..] $ getLaserOrder orderedAngles

  let (x,y) = M.findWithDefault (error "wut?") 200 lasered

  print $ x*100 + y