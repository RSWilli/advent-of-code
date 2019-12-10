import System.IO
import Data.List
import qualified Computer as C

testFile :: IO [C.Memory]
testFile = do 
    handle <- openFile "./test.txt" ReadMode  
    contents <- hGetContents handle
    let programs = lines contents
    return $ map C.parseIntcodeProgram programs

inputFile :: IO C.Memory
inputFile = do 
    handle <- openFile "./input.txt" ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

main = do
    memory <- inputFile
    print $ C.runIntcodeToList memory [1]
    print $ C.runIntcodeToList memory [2]