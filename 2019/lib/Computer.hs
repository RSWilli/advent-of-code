{-# Language OverloadedStrings #-}
module Computer
  ( -- * Machine state
    Machine(Machine)
  , (!)
  , new
  , set
  , memory
  ,

    -- * Effects
    Effect(Halt, Input, Output)
  , run
  , effectToList
  , memoryParser
  , runIntcodeToList
  , Datatype
  , Program
  )
where

import qualified Data.Map.Strict               as M
import           InputParser                   (Parser, number, sepBy)

type Datatype = Integer

type Program = [Datatype]

type Memory = M.Map Datatype Datatype

data Machine = Machine
  { pc      :: Datatype
  , memory  :: Memory
  , relativeBase :: Datatype
  }

new :: Program -> Machine
new pgr = 
  let mem = M.fromList $ zip [0..] pgr
  in
    Machine { memory = mem, pc = 0, relativeBase = 0 }

data Effect = Halt Machine
             | Input (Datatype -> Effect)
             | Output Datatype Effect

runIntcodeToList :: Program -> [Datatype] -> [Datatype]
runIntcodeToList mem = effectToList (run $ new mem)

effectToList :: Effect -> [Datatype] -> [Datatype]
effectToList effect inputs = case effect of
  Input f | x : xs <- inputs -> effectToList (f x) xs
          | otherwise        -> error "Not enough inputs"
  Output v machine' -> v : effectToList machine' inputs
  Halt _            -> []

run :: Machine -> Effect
run machine = case step machine of
  Step   machine'      -> run machine'
  StepIn f             -> Input (run . f)
  StepOut val machine' -> Output val (run machine')
  StepHalt m           -> Halt m

data Step = Step Machine
          | StepIn (Datatype -> Machine)
          | StepOut Datatype Machine
          | StepHalt Machine

step :: Machine -> Step
step machine = result machine
 where
  val (Pos i) = machine ! i
  val (Imm i) = i
  val (Rel i) = machine ! (relativeBase machine + i)

  save (Pos i) = set i
  save (Rel i) = set (relativeBase machine + i)
  save (Imm _) = error "immediate saving not allowed"

  result = case decode machine of
    Add a b c -> Step . adv 4 . save c (val a + val b)
    Mul a b c -> Step . adv 4 . save c (val a * val b)
    In  a     -> \m -> StepIn (\input -> adv 2 (save a input m))
    Out a     -> StepOut (val a) . adv 2
    Jnz a b | val a /= 0 -> Step . jmp (val b)
            | otherwise  -> Step . adv 3
    Jz a b | val a == 0 -> Step . jmp (val b)
           | otherwise  -> Step . adv 3
    Lt a b c -> Step . adv 4 . save c (if val a < val b then 1 else 0)
    Eq a b c -> Step . adv 4 . save c (if val a == val b then 1 else 0)
    AdjRel a -> Step . adv 2 . adjustBase (val a)
    Hlt      -> StepHalt

memoryParser :: Parser [Datatype]
memoryParser = number `sepBy` ","

-- advance pc
adv :: Datatype -> Machine -> Machine
adv i m = m { pc = pc m + i }

-- jump to pc
jmp :: Datatype -> Machine -> Machine
jmp addr m = m { pc = addr }

adjustBase :: Datatype -> Machine -> Machine
adjustBase change m = m { relativeBase = relativeBase m + change }

set :: Datatype -> Datatype -> Machine -> Machine
set pos value m = if pos < 0 then error "write to negative index"
  else m { memory = M.insert pos value (memory m) }

-- get, no negative index, defaults to 0
(!) :: Machine -> Datatype -> Datatype
m !i = if i < 0 then error "access at negative index"
  else M.findWithDefault 0 i (memory m)

-- get the digit at position i
digit
  :: Datatype {- ^ position -}
  -> Datatype {- ^ number -}
  -> Datatype {- ^ digit -}
digit i x = x `div` (10 ^ i) `mod` 10

data Param = Pos Datatype
           | Imm Datatype
           | Rel Datatype

data Opcode = Add Param Param Param
            | Mul Param Param Param
            | In Param
            | Out Param
            | Jnz Param Param
            | Jz Param Param
            | Lt Param Param Param
            | Eq Param Param Param
            | AdjRel Param
            | Hlt

decode :: Machine -> Opcode
decode machine =
  let
    arg i = machine ! (pc machine + i)

    -- opcode are the last 2 digits
    opcode = arg 0 `mod` 100

    -- get the operand mode for operand i
    mode i = digit (i + 1) (arg 0)

    -- get the parameter for parameter i
    param i = case mode i of
      0 -> Pos (arg i)
      1 -> Imm (arg i)
      2 -> Rel (arg i)
      x -> error $ "bad parameter mode: " ++ show x
  in
    case opcode of
          -- add
      1  -> Add (param 1) (param 2) (param 3)
      -- multiply
      2  -> Mul (param 1) (param 2) (param 3)
      -- input
      3  -> In (param 1)
      -- output
      4  -> Out (param 1)
      -- jump-if-true
      5  -> Jnz (param 1) (param 2)
      -- jump-if-false
      6  -> Jz (param 1) (param 2)
      -- less-than
      7  -> Lt (param 1) (param 2) (param 3)
      -- equal-to
      8  -> Eq (param 1) (param 2) (param 3)
      -- adjust relative base
      9 -> AdjRel (param 1)
      99 -> Hlt
      x ->
        error $ "undefined opcode: " ++ show x ++ " parsed from " ++ show
          (arg 0)
