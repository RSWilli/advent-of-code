{-# language LambdaCase #-}

import           System.IO
import           Data.List.Split
import           Data.List
import           Control.Monad                  ( guard )
import qualified Data.Map.Strict               as M

data Direction = U | L | D | R deriving (Show)
type RelativePath = [(Direction, Int)]

type Position = (Int, Int)
type Way = (Position, Position)
type AbsolutePath = [Position]

parseFile :: String -> IO [RelativePath]
parseFile path = do
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    let pathStrings = lines contents
    let paths       = map (splitOn ",") pathStrings :: [[String]]

    let path = map
            (map
                (\(x : xs) ->
                    let convert = \case
                            'U' -> U
                            'L' -> L
                            'D' -> D
                            'R' -> R
                            x   -> error "unknown direction"
                    in  (convert x, read xs :: Int)
                )
            )
            paths
    return path

testFile :: IO [RelativePath]
testFile = parseFile "./test.txt"

inputFile :: IO [RelativePath]
inputFile = parseFile "../input.txt"

createAbsolutePath :: RelativePath -> AbsolutePath
createAbsolutePath = reverse . foldl
    (\path@((x, y) : _) (direction, distance) ->
        let newpos = case direction of
                U -> (x, y + distance)
                D -> (x, y - distance)
                L -> (x - distance, y)
                R -> (x + distance, y)
        in  newpos : path
    )
    [(0, 0)]

normalizeWay :: Way -> Way
normalizeWay (p1, p2) = case compare p1 p2 of
        LT -> (p1, p2)
        GT -> (p2, p1)
        EQ -> error "no way"

getWays :: AbsolutePath -> [Way]
getWays (start : second : rest) = reverse $ map normalizeWay $ foldl
    (\ways@((_, (curX, curY)) : _) pos -> ((curX, curY), pos) : ways)
    [(start, second)]
    rest

getPointsOnWay :: Way -> [Position]
getPointsOnWay ((x1, y1), (x2, y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

-- getIntersections :: [Way] -> [Way] -> [Position]
getIntersections w1 w2 = let {
            pointsW1 = concatMap getPointsOnWay w1 ;
            pointsW2 = concatMap getPointsOnWay w2
    } in pointsW1 `intersect` pointsW2

main = do
    paths <- testFile
    let (p1:p2:_) = map (getWays . createAbsolutePath) paths
    let intersections = getIntersections p1 p2
    let distances = sort $ map (uncurry (+)) intersections
    print distances