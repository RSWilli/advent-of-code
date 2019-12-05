module Computer
    ( Memory
    , runProgram
    , createState
    )
where

import qualified Data.Map.Strict               as M

type Memory = M.Map Int Int

type Machinestate = Maybe (Int, Memory)

saveAt :: Int -> Int -> Memory -> Memory
saveAt = M.insert

getOpWithMode :: Int -> Memory -> Int -> Maybe Int
getOpWithMode addr memory mode = do
    op <- M.lookup addr memory
    if mode == 0 then
        M.lookup op memory
    else
        return op

getOps3 :: Machinestate -> Int -> Int -> Int -> Maybe (Int, Int, Int)
getOps3 state mode1 mode2 mode3 = do
    (pc, memory) <- state

    op1          <- getOpWithMode (pc+1) memory mode1
    op2          <- getOpWithMode (pc+2) memory mode2
    op3          <- getOpWithMode (pc+3) memory mode3

    return (op1, op2, op3)

getOps1 :: Machinestate -> Int -> Maybe Int
getOps1 state mode = do
    (pc, memory) <- state

    getOpWithMode (pc+1) memory mode

parseInstruction :: Int -> (Int, Int, Int, Int)
parseInstruction instruction =
    let opcode   = instruction `mod` 100
        rest1    = instruction `div` 100

        mode1    = rest1 `mod` 10
        restmode = rest1 `div` 10

        mode2    = restmode `mod` 10
        mode3    = restmode `div` 10
    in  (opcode, mode1, mode2, mode3)

runProgram :: Machinestate -> Machinestate
runProgram state = do
    (pc, memory) <- state
    instruction  <- M.lookup pc memory

    let (opcode, mode1, mode2, mode3) = parseInstruction instruction

    case opcode of
        1 -> do
            (op1, op2, res) <- getOps3 state mode1 mode2 1
            let newstate = return (pc + 4, saveAt res (op1 + op2) memory)
            runProgram newstate
        2 -> do
            (op1, op2, res) <- getOps3 state mode1 mode2 1
            let newstate = return (pc + 4, saveAt res (op1 * op2) memory)
            runProgram newstate
        99 -> state
        x  -> error $ "undefined opcode: " ++ show x

createState :: Memory -> Machinestate
createState mem = return (0, mem)
