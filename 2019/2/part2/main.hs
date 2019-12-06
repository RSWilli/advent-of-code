import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map.Strict as M

import qualified Computer as C

import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

parseFile :: String -> IO C.Memory
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    let opcodes = splitOn "," contents
    let memory = M.fromList $ zipWith (\i op -> (i, read op :: Int)) [0..] opcodes
    return memory

testFile :: IO C.Memory
testFile = parseFile "./test.txt"

inputFile :: IO C.Memory
inputFile = parseFile "../input.txt"

changeInputs :: C.Memory -> Int -> Int -> C.Memory
changeInputs memory p1 p2 = M.insert 1 p1 $ M.insert 2 p2 memory

bruteForce :: [(Int, Int)] -> C.Memory -> Int -> MaybeT IO (Int, Int)
bruteForce parameterList mem desiredResult = case parameterList of
    [] -> liftMaybe Nothing
    current:next -> do
        let (p1, p2) = current
        let modifiedMem = changeInputs mem p1 p2
        resultMem <- C.runProgram $ C.createState modifiedMem 
        result <- liftMaybe $ M.lookup 0 $ snd resultMem
        
        if result == desiredResult then
            return current
        else
            bruteForce next mem desiredResult

main = do  
    memory <- inputFile
    let parameterRange = [0..99]
    let paramterCombinations = do 
        x <- parameterRange
        y <- parameterRange
        return (x,y)
    putStrLn $ show paramterCombinations
    neededParams <- runMaybeT $ bruteForce paramterCombinations memory 19690720
    case neededParams of
        Nothing -> putStrLn "Error in Computation"
        Just (p1, p2) -> do 
            putStrLn $ show $ p1 * 100 + p2