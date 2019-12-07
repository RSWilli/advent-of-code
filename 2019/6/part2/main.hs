import           Data.List.Split
import           System.IO
import qualified Data.Map                      as M
import           Control.Applicative

type Orbits = M.Map String [String]

parseInput :: String -> Orbits
parseInput str =
    let ls = lines str
        kv = map (splitOn ")") ls
    in  M.fromListWith (++)
            $ concatMap (\(x : y : _) -> [(x, [y]), (y, [x])]) kv

findPath :: String -> String -> Orbits -> [String] -> Maybe [String]
findPath current destination graph visited =
    let newVisited = current : visited
    in
        if current == destination
            then return newVisited
            else do
                connectedNodes <- M.lookup current graph
                let next = filter (`notElem` visited) connectedNodes
                let
                let paths =
                        map (\v -> findPath v destination graph newVisited) next

                foldr (liftA2 (++)) (return []) paths


main = do
    handle   <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let orbits = parseInput contents
    print orbits
    let path = findPath "YOU" "SAN" orbits []
    print path
    print $ (subtract 3) . length <$> path
