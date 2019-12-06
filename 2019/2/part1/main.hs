import System.IO
import Data.List
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe 

import qualified Computer as C


parseFile :: String -> IO (C.Memory)
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

testFile :: IO C.Memory
testFile = parseFile "./test.txt"

inputFile :: IO C.Memory
inputFile = do
    memory <- parseFile "../input.txt"
    let modifiedMem = M.insert 1 12 $ M.insert 2 2 memory
    return modifiedMem

main = do  
    memory <- testFile
    finishedProg <- runMaybeT $ C.runProgram $ C.createState memory
    case finishedProg of
        Nothing -> putStrLn "Error in Computation"
        Just (_, mem) -> do 
            putStrLn $ show $ M.lookup 0 mem
            putStrLn $ show $ map snd $ M.toList mem