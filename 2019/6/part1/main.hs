import Data.List.Split
import System.IO
import qualified Data.Map as M
import Control.Applicative


type Orbits = M.Map String [String]

parseInput :: String -> Orbits
parseInput str = 
    let ls = lines str
        kv = map (splitOn ")") ls
        orbits = M.fromListWith (++)
            $ map 
                (\(x:y:_) -> (x,[y])) 
                kv
        in
            orbits
        

countOrbits :: Orbits -> String -> Maybe Int -> Maybe Int
countOrbits orbits key count =
    case M.lookup key orbits of
        Nothing -> count
        Just children -> do
            let childrenCount = map (\newkey -> countOrbits orbits newkey ((+1) <$> count)) children

            let childrenSum = foldr (liftA2 (+)) (return 0) childrenCount

            (+) <$> count <*> childrenSum

main = do
        handle <- openFile "../input.txt" ReadMode  
        contents <- hGetContents handle
        let orbits = parseInput contents
        print orbits
        print $ countOrbits orbits "COM" (Just 0)
        print $ M.lookup "COM" orbits
