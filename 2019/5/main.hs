import System.IO
import Data.List
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe 

import qualified Computer as C


parseFile :: String -> IO (M.Map Int Int)
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

main = do  
    memory <- parseFile "./input.txt"
    let running = C.run $ C.machine memory
    let part1 = last $ C.effectToList running [1]
    let part2 = last $ C.effectToList running [5]
    print $ "Part1: " ++ show part1
    print $ "Part2: " ++ show part2