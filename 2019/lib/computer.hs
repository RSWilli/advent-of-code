module Computer (Memory, runProgram, createState)
where

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

createState :: Memory -> Machinestate
createState mem = return (0, mem)