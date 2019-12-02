import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map.Strict as M

type Memory = M.Map Int Int

type Machinestate = Maybe (Int, Memory)

saveAt :: Int -> Int-> Memory -> Memory
saveAt = M.insert

getOps :: Machinestate -> Maybe(Int, Int, Int)
getOps state = do
    (pc, memory) <- state
    op1addr <- M.lookup (pc + 1) memory
    op2addr <- M.lookup (pc + 2) memory
    resaddr <- M.lookup (pc + 3) memory

    op1 <- M.lookup op1addr memory
    op2 <- M.lookup op2addr memory

    return (op1, op2, resaddr)

runProgram :: Machinestate -> Machinestate
runProgram state = do
    let ops = getOps state
    (pc, memory) <- state
    op <- M.lookup pc memory 
    case op of
        1 -> do
            (op1, op2, res) <- ops
            let newstate = return (pc + 4, saveAt res (op1 + op2) memory)
            runProgram newstate
        2 -> do
            (op1, op2, res) <- ops
            let newstate = return (pc + 4, saveAt res (op1 * op2) memory)
            runProgram newstate
        99 -> state
        x -> error $ "undefined opcode: " ++ show x

parseFile :: String -> IO (Memory)
parseFile path = do 
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    let opcodes = splitOn "," contents
    let memory = M.fromList $ zipWith (\i op -> (i, read op :: Int)) [0..] opcodes
    return memory

testFile :: IO(Memory)
testFile = parseFile "./test.txt"

inputFile :: IO(Memory)
inputFile = do
    memory <- parseFile "../input.txt"
    let modifiedMem = M.insert 1 12 $ M.insert 2 2 memory
    return modifiedMem

main = do  
    memory <- inputFile
    let finishedProg = runProgram $ Just (0, memory)
    case finishedProg of
        Nothing -> putStrLn "Error in Computation"
        Just (_, mem) -> do 
            putStrLn $ show $ M.lookup 0 mem
            putStrLn $ show $ map snd $ M.toList mem