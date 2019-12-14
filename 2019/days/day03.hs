{-# Language OverloadedStrings #-}
import           System.IO
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import InputParser
import Data.Foldable (asum)

type Field = M.Map Position Int
type Position = (Int, Int)

data Direction = U | R | L | D deriving Show
type Step = (Direction, Int)
type Wire = [Step]

parseDirection :: Parser Direction
parseDirection = asum [ U <$ "U"
                      , L <$ "L"
                      , R <$ "R"
                      , D <$ "D"
                      ]

parseStep :: Parser Step
parseStep = (,) <$> parseDirection <*> number

parseWire :: Parser Wire
parseWire = parseStep `sepBy` ","

getDestination :: Position -> Step -> Position
getDestination (x, y) step = case step of
  (U, distance) -> (x, y + distance)
  (D, distance) -> (x, y - distance)
  (L, distance) -> (x - distance, y)
  (R, distance) -> (x + distance, y)

createPath :: Wire -> Field
createPath wire = 
  snd 
  $ foldl (\(pos, map) step ->
            let destination = getDestination pos step
                Just stepcount = M.lookup pos map
                interPoses = getIntermediatePositions pos destination
                interSteps = zip interPoses [stepcount..]
                newmap     = foldl (\map (pos, interstep) -> M.insertWith (\_ oldval -> oldval) pos interstep map) map interSteps       
            in  (destination, newmap)
          )          
          ((0, 0), M.singleton (0,0) 0)
          wire


-- generate all intermediate positions between two positions
getIntermediatePositions :: Position -> Position -> [Position]
getIntermediatePositions x@(x1, x2) y@(y1, y2) = case compare x y of
  LT -> [ (x, y) | x <- [x1 .. y1], y <- [x2 .. y2] ]
  GT -> [ (x, y) | x <- [x1, x1-1 .. y1], y <- [x2, x2-1 .. y2] ]
  EQ -> error "no way"


main :: IO ()
main = do
  [wire1, wire2] <- getParsedLines 3 parseWire
  let p1 = createPath wire1
  let p2 = createPath wire2
  let k1 = M.keysSet p1
  let k2 = M.keysSet p2

  -- intersect both keysets, but exclude (0,0)
  let intersections = S.delete (0,0) $ S.intersection k1 k2


  let distances = S.map (\(x,y) -> abs x + abs y) intersections
  putStrLn "part1:"
  print $ minimum distances
  let combinedSteps = map (\pos -> M.findWithDefault (-1) pos p1 + M.findWithDefault (-1) pos p2) $ S.toList $ intersections
  putStrLn "part2:"
  print $ minimum combinedSteps
