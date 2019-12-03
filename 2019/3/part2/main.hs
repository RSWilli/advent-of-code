{-# language LambdaCase #-}

import           System.IO
import           Data.List.Split
import           Data.List
import qualified Data.Map                      as M
import qualified Data.Set                      as S

type Field = M.Map Position Int
type Position = (Int, Int)

getDestination :: Position -> String -> Position
getDestination (x, y) step = case step of
    'U' : distance -> (x, y + read distance)
    'D' : distance -> (x, y - read distance)
    'L' : distance -> (x - read distance, y)
    'R' : distance -> (x + read distance, y)

parseFile :: String -> IO [Field]
parseFile path = do
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    let pathStrings = lines contents
    let paths       = map (splitOn ",") pathStrings :: [[String]]
    print paths

    let fields = map
            (snd . foldl
                (\(pos, map) step ->
                    let destination = getDestination pos step
                        Just stepcount = M.lookup pos map
                        steps       = zip (getSteps pos destination) [stepcount..]
                        newmap      = foldl (\map (pos, step) -> M.insertWith (\_ oldval -> oldval) pos step map) map steps
                    in  (destination, newmap)
                )
                ((0, 0), M.singleton (0,0) 0)
            )
            paths
    return fields

testFile :: IO [Field]
testFile = parseFile "./test.txt"

inputFile :: IO [Field]
inputFile = parseFile "../input.txt"

getSteps :: Position -> Position -> [Position]
getSteps x@(x1, x2) y@(y1, y2) = case compare x y of
    LT -> [ (x, y) | x <- [x1 .. y1], y <- [x2 .. y2] ]
    GT -> [ (x, y) | x <- [x1, x1-1 .. y1], y <- [x2, x2-1 .. y2] ]
    EQ -> error "no way"

main = do
    p1:p2:_ <- inputFile
    let k1 = M.keysSet p1
    let k2 = M.keysSet p2
    let distances = map (\pos -> M.findWithDefault (-1) pos p1 + M.findWithDefault (-1) pos p2) $ S.toList $ S.intersection k1 k2
    print $ sort distances
