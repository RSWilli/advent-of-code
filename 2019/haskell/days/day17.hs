import qualified Computer as C
import InputParser
import Util
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (chr, ord)

program :: String
program = unlines [
    "A,B,A,A,B,C,B,C,C,B",
    "L,12,R,8,L,6,R,8,L,6",
    "R,8,L,12,L,12,R,8",
    "L,6,R,6,L,12",
    "n"
  ]

main :: IO ()
main = do
  [pgrm] <- getParsedLines 17 C.memoryParser
  let asciis = map (chr . fromIntegral) $ C.runIntcodeToList pgrm []
  let display = twoDArrayToMap $ lines asciis
  
  let coords = M.keysSet display
  
  -- print coords
  let intersections = S.filter (\pos ->
                                  let neighs = neighbors pos
                                      isScaffold = ('#' ==)
                                      isPositionScaffold pos = case M.lookup pos display of
                                                                  Nothing -> False
                                                                  Just c -> isScaffold c
                                  in
                                    isPositionScaffold pos && all isPositionScaffold neighs
                               ) 
                               coords
      
      
  putStr asciis

  let aligmentParams = S.foldr ((+) . uncurry (*)) 0 intersections

  print aligmentParams

  let part2pgrm = 2 : tail pgrm

  print $ C.runIntcodeToList part2pgrm (map (fromIntegral.ord) program)
