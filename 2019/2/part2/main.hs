import System.IO
import Data.List
import qualified Data.Map.Strict as M

import qualified Computer as C

import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

parseFile :: String -> IO (M.Map Int Int)
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    return $ C.parseIntcodeProgram contents

testFile :: IO (M.Map Int Int)
testFile = parseFile "./test.txt"

inputFile :: IO (M.Map Int Int)
inputFile = parseFile "../input.txt"

changeInputs :: M.Map Int Int -> Int -> Int -> M.Map Int Int
changeInputs memory p1 p2 = M.insert 1 p1 $ M.insert 2 p2 memory

bruteForce :: [(Int, Int)] -> M.Map Int Int -> Int -> Maybe (Int, Int)
bruteForce parameterList mem desiredResult = case parameterList of
    [] -> Nothing
    current:next -> do
        let (p1, p2) = current
        let modifiedMem = changeInputs mem p1 p2
        let finishedState = C.run $ C.machine modifiedMem 
        case finishedState of
            C.Halt machine -> do
                let result = machine C.! 0
                
                if result == desiredResult then
                    return current
                else
                    bruteForce next mem desiredResult
            _ -> Nothing

main = do  
    memory <- inputFile
    let parameterRange = [0..99]
    let parameterCombinations = do 
        x <- parameterRange
        y <- parameterRange
        return (x,y)
    let neededParams = bruteForce parameterCombinations memory 19690720
    case neededParams of
        Nothing -> putStrLn "Error in Computation"
        Just (p1, p2) -> do 
            putStrLn $ show $ p1 * 100 + p2