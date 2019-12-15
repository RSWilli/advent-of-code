import InputParser

import qualified Computer as C

main = do  
  [machine] <- map C.new <$> getParsedLines 5 C.memoryParser
  let running = C.run machine
  let part1 = last $ C.effectToList running [1]
  let part2 = last $ C.effectToList running [5]
  print $ "Part1: " ++ show part1
  print $ "Part2: " ++ show part2