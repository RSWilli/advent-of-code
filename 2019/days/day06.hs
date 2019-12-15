{-# Language OverloadedStrings #-}
import qualified Data.Map as M
import Control.Applicative
import InputParser

type Orbits = M.Map String [String]

type Orbit = (String, [String])

nameParser :: Parser String
nameParser = many $ satisfy ( ')' /= )

orbitParser :: Parser Orbit
orbitParser = (,) <$> nameParser <* ")" <*> ((:[]) <$> nameParser)

countOrbits :: Orbits -> String -> Maybe Int -> Maybe Int
countOrbits orbits key count =
  case M.lookup key orbits of
    Nothing -> count
    Just children -> do
      let childrenCount = map (\newkey -> countOrbits orbits newkey ((+1) <$> count)) children

      let childrenSum = foldr (liftA2 (+)) (return 0) childrenCount

      (+) <$> count <*> childrenSum

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
        let paths = map (\v -> findPath v destination graph newVisited) next

        foldr (liftA2 (++)) (return []) paths

main = do
  input <- getParsedLines 6 orbitParser
  let orbitsTree = M.fromListWith (++) input
  print "Part1:"
  print $ countOrbits orbitsTree "COM" (Just 0)
  
  let orbitsGraph = M.fromListWith (++) $ concatMap (\(src, [dest]) -> [(src, [dest]), (dest, [src])]) input
  let path = findPath "YOU" "SAN" orbitsGraph []
  print "Part2:"
  print $ (subtract 3) . length <$> path