import System.IO
import System.Process
import Data.List
import Control.Monad
import Data.Function (fix)

import qualified Computer as C

type ListFn = [Int] -> [Int]

parseFile :: String -> IO C.Memory
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

testFile :: IO C.Memory
testFile = parseFile "./test.txt"

inputFile :: IO C.Memory
inputFile = parseFile "./input.txt"

main = do
    memory <- inputFile
    print $ part1 $ C.runIntcodeToList memory
    print $ part2 $ C.runIntcodeToList memory

part1 :: ListFn -> Int
part1 pgm = maximum [head (thrustController pgm p) | p <- permutations [0..4]]

part2 :: ListFn -> Int
part2 pgm = maximum [last (thrustController pgm p) | p <- permutations [5..9]]

thrustController :: ListFn -> ListFn
thrustController ctrl phases = tieknot [ctrl << p | p <- phases]

tieknot :: [ListFn] -> [Int]
tieknot fs = fix (composeLR fs << 0)


-- compose all functions together, output of one is input for the next
composeLR :: [a -> a] -> (a -> a)
composeLR = foldl (flip (.)) id


-- feed: 
(<<) :: (ListFn) -> Int -> ListFn
(f << x) xs = f (x:xs)