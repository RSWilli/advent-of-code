import System.IO
import Data.List
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe 

import qualified Computer as C


parseFile :: String -> IO C.Memory
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

main = do  
    memory <- parseFile "./input.txt"
    runMaybeT $ C.runProgram $ C.createState memory