import System.IO
import Data.List
import qualified Data.Map as M

import qualified Computer as C


parseFile :: String -> IO (M.Map Int Int)
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

testFile :: IO (M.Map Int Int)
testFile = parseFile "./test.txt"

inputFile :: IO (M.Map Int Int)
inputFile = do
    memory <- parseFile "../input.txt"
    let modifiedMem = M.insert 1 12 $ M.insert 2 2 memory
    return modifiedMem

main = do  
    memory <- inputFile
    let finalState = C.run $ C.machine memory
    case finalState of
        C.Halt machine -> do 
            putStrLn $ show $ M.lookup 0 (C.memory machine)
        _ -> putStrLn "Error in Computation"