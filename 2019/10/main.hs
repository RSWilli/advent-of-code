import Data.Function (fix)
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO
import Data.List
import Data.Maybe (fromMaybe)

data AsteroidField = AsteroidField {
  width :: Int, 
  height :: Int, 
  field :: S.Set (Int, Int)
} deriving (Show)

index :: [a] -> [(Int, a)]
index = zip [0..]

parse :: String -> AsteroidField
parse input = let ls = lines input
                  height = length ls
                  width = length $ head ls

                  pairList  = concatMap 
                    (\(y, row) ->
                      map (\(x,y, char) -> (x,y))
                      $
                        filter (\(_,_,c) -> c == '#') 
                        $ map 
                          (\(x, char) -> (x,y,char)) 
                          $ index row
                    ) 
                    $ index ls
                  in
                    AsteroidField {width = width, height = height, field = S.fromList pairList}

shadow :: (Int, Int) -> (Int, Int) -> AsteroidField -> AsteroidField
shadow start@(startX, startY) (astX, astY) asteroidfield =
  let w = width asteroidfield
      h = height asteroidfield

      diffX = astX - startX
      diffY = astY - startY

      g = gcd diffX diffY

      deltaX = diffX `div` g
      deltaY = diffY `div` g

      shadowedPositions = getPositions deltaX deltaY w h astX astY

      newastfield = foldl (\set pos -> S.delete pos set) (field asteroidfield) shadowedPositions
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
                                        (\astfield ast -> shadowAsteroid ast astfield) 
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
getAngle sx sy x y = let ang = pi/2 + (atan2 (fromIntegral $ y-sy) (fromIntegral $ x-sx))
                         in
                           if ang > 2*pi then ang - 2*pi else if ang < 0 then ang + 2* pi else ang

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
  y:ys -> y : (getLaserOrder $ xs ++ [ys])

main = do
  handle <- openFile "./input.txt" ReadMode
  contents <- hGetContents handle
  let astf = parse contents
  let (bestVis, bestX, bestY) = maximum $ getAllViews $ astf
  print bestVis
  print bestX
  print bestY 

  let orderFn = orderAngles bestX bestY
  let eqFn = angleEqual bestX bestY

  let orderDistFn = orderDistance bestX bestY

  let orderedAngles = map (sortBy orderDistFn) $ groupBy eqFn $ sortBy orderFn $ S.toList $ field astf

  print $ getLaserOrder orderedAngles

  let lasered = M.fromList $ zip [1..] $ getLaserOrder orderedAngles

  let (x,y) = fromMaybe (error "wut?") (M.lookup 200 lasered)

  print $ x*100 + y