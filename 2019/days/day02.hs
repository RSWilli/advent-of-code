import System.IO
import InputParser

import qualified Computer as C

changeInputs :: C.Machine -> C.Datatype -> C.Datatype -> C.Machine
changeInputs memory p1 p2 = C.set 1 p1 $ C.set 2 p2 memory

bruteForce :: [(C.Datatype, C.Datatype)] -> C.Machine -> C.Datatype -> Maybe (C.Datatype, C.Datatype)
bruteForce parameterList mem desiredResult = case parameterList of
  [] -> Nothing
  current:next -> do
    let (p1, p2) = current
    let modified = changeInputs mem p1 p2
    let finishedState = C.run modified 
    case finishedState of
      C.Halt machine -> do
        let result = machine C.! 0
        
        if result == desiredResult then
          return current
        else
          bruteForce next mem desiredResult
      _ -> Nothing

main = do  
  [machine] <- map C.new <$> getParsedLines 2 C.memoryParser
  part1 machine
  part2 machine

part1 :: C.Machine -> IO ()
part1 mach = do
  let modifiedMem = changeInputs mach 12 2
  let finalState = C.run modifiedMem
  case finalState of
    C.Halt machine -> print $ machine C.! 0
    _ -> putStrLn "Error in Computation"

part2 :: C.Machine -> IO ()
part2 mach = do
  let parameterRange = [0..99]
  let parameterCombinations = do x <- parameterRange
                                 y <- parameterRange
                                 return (x,y)
  let neededParams = bruteForce parameterCombinations mach 19690720
  case neededParams of
    Nothing -> putStrLn "Error in Computation"
    Just (p1, p2) -> print $ p1 * 100 + p2