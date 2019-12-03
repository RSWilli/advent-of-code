{-# language LambdaCase #-}

import           System.IO
import           Data.List.Split
import           Data.List
import qualified Data.Set               as S

type Field = S.Set Position
type Position = (Int, Int)

getDestination :: Position -> String -> Position
getDestination (x,y) step = case step of
    'U':distance -> (x, y + read distance)
    'D':distance -> (x, y - read distance)
    'L':distance -> (x - read distance, y)
    'R':distance -> (x + read distance, y)

parseFile :: String -> IO [Field]
parseFile path = do
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    let pathStrings = lines contents
    let paths       = map (splitOn ",") pathStrings :: [[String]]
    print paths

    let fields = map (snd.foldl 
                (\(pos,set) step ->
                    let destination = getDestination pos step
                        steps = getSteps pos destination
                        newset = foldl (flip S.insert) set steps
                        in
                            (destination, newset)
                )
                ((0,0), S.empty)
            )
            paths
    return fields

testFile :: IO [Field]
testFile = parseFile "./test.txt"

inputFile :: IO [Field]
inputFile = parseFile "../input.txt"

getSteps :: Position -> Position -> [Position]
getSteps x y = 
    let ((x1,x2), (y1,y2)) = case compare x y of
            LT -> (x,y)
            GT -> (y,x)
            EQ -> error "no way"
        in
            [(x,y) | x <- [x1..y1], y <- [x2..y2]]
    
main = do
    p1:p2:_ <- inputFile
    let intersections = p1 `S.intersection` p2
    let distances = S.map (\(x,y) -> abs x + abs y) intersections
    print distances
