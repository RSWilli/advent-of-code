module Computer
    ( -- * Machine state
    Machine( Machine ), (!), machine, set, memory, 
  
    -- * Effects
    Effect( Halt, Input, Output), run

    -- constructor
    , parseIntcodeProgram
    )
where

import qualified Data.Map.Strict               as M
import Data.List.Split
import Data.Bool (bool)

type Memory = M.Map Int Int

data Machine = Machine
  { pc      :: Int
  , memory  :: M.Map Int Int
  }

machine :: Memory -> Machine
machine mem = Machine {memory = mem, pc = 0}

data Effect = Halt Machine
             | Input (Int -> Effect)
             | Output Int Effect

run :: Machine -> Effect
run machine = 
  case step machine of
    Step machine' -> run machine'
    StepIn f -> Input (run . f)
    StepOut val machine' -> Output val (run machine')
    StepHalt m -> Halt m

data Step = Step Machine
          | StepIn (Int -> Machine)
          | StepOut Int Machine
          | StepHalt Machine

step :: Machine -> Step
step machine = result machine
  where
    val (Pos i) = machine ! i
    val (Imm i) = i

    save (Pos i) = set i
    save (Imm _) = error "immediate saving not allowed"

    result = case decode machine of
        Add a b c -> Step . adv 4 . save c (val a + val b)
        Mul a b c -> Step . adv 4 . save c (val a * val b)
        In a      -> \m -> StepIn (\input -> adv 2 ( save a input m))
        Out a     -> StepOut (val a) . adv 2
        Jnz a b   | (val a /= 0) -> Step . jmp (val b)
                  | otherwise -> Step . adv 3
        Jz a b    | (val a == 0) -> Step . jmp (val b)
                  | otherwise -> Step . adv 3
        Lt a b c  -> Step . adv 4 . save c (bool 0 1 (val a <  val b))
        Eq a b c  -> Step . adv 4 . save c (bool 0 1 (val a == val b))
        Hlt      -> StepHalt

parseIntcodeProgram :: String -> Memory
parseIntcodeProgram program = M.fromList $
  zipWith 
    (\i op -> (i, read op :: Int)) 
    [0..] 
    (splitOn "," program)

-- advance pc
adv :: Int -> Machine -> Machine
adv i m = m { pc = pc m + i }

-- jump to pc
jmp :: Int -> Machine -> Machine
jmp addr m = m { pc = addr }

set :: Int -> Int -> Machine -> Machine
set pos value m = m {memory = M.insert pos value (memory m)}

-- get
(!) :: Machine -> Int -> Int
m ! i = case M.lookup i (memory m) of
  Just x -> x
  Nothing -> error "no index found"

-- get the digit at position i
digit :: Int {- ^ position -} -> Int {- ^ number -} -> Int {- ^ digit -}
digit i x = x `div` (10^i) `mod` 10

data Param = Pos Int
           | Imm Int

data Opcode = Add Param Param Param
            | Mul Param Param Param
            | In Param
            | Out Param
            | Jnz Param Param
            | Jz Param Param
            | Lt Param Param Param
            | Eq Param Param Param
            | Hlt

decode :: Machine -> Opcode
decode machine = 
    let 
      arg i = machine ! (pc machine + i)
      
      -- opcode are the last 2 digits
      opcode = arg 0 `mod` 100

      -- get the operand mode for operand i
      mode i = digit i (arg 0)

      -- get the parameter for parameter i
      param i = case mode i of
        0 -> Pos (arg i)
        1 -> Imm (arg i)
        x -> error $ "bad parameter mode: " ++ show x

      in
        case opcode of
            -- add
          1 -> Add (param 1) (param 2) (param 3)
          -- multiply
          2 -> Mul (param 1) (param 2) (param 3)
          -- input
          3 -> In (param 1)
          -- output
          4 -> Out (param 1)
          -- jump-if-true
          5 -> Jnz (param 1) (param 2)
          -- jump-if-false
          6 -> Jz (param 1) (param 2)
          -- less-than
          7 -> Lt (param 1) (param 2) (param 3)
          -- equal-to
          8 -> Eq (param 1) (param 2) (param 3)
          99 -> Hlt
          x  -> error $ "undefined opcode: " ++ show opcode ++ " parsed from " ++ show (arg 0)
