import qualified Computer as C
import qualified Data.Map as M
import Util
import InputParser

type Field = M.Map (Integer, Integer) Integer

main :: IO ()
main = do
  [program] <- getParsedLines 13 C.memoryParser
  part1 program
  part2 (C.new program)

getWidth :: Field -> Integer
getWidth field = 
  let (minX, maxX) = foldl (\(minX, maxX) (_, x) -> (min minX x, max maxX x)) (1000, 0) $ M.keys field
  in
    maxX - minX + 1

replaceTileID :: Integer -> String
replaceTileID i = case i of
  0 -> " "
  1 -> "█"
  2 -> "x"
  3 -> "_"
  4 -> "o"
  _ -> error "unknown tile"

printField :: Field -> String
printField field = unlines $ chunks (getWidth field) $ concatMap (replaceTileID.snd) $ M.toList field


part1 :: C.Program -> IO ()
part1 program = do
  let triples = chunks 3 $ C.runIntcodeToList program []
  let field = foldl (\f [x,y,tileId] -> M.insert (y,x) tileId f) M.empty triples
  let triples = chunks 3 $ C.runIntcodeToList program []
  let field = foldl (\f [x,y,tileId] -> M.insert (y,x) tileId f) M.empty triples
  putStr $ printField field
  print $ length $ filter ((2==).snd) $ M.toList field



part2 :: C.Machine -> IO  ()
part2 program = do
  let programWithJoystick = C.set 0 2 program

  let score = autoPlay 0 0 0 (C.run programWithJoystick)

  print score



autoPlay :: C.Datatype -> C.Datatype -> C.Datatype -> C.Effect -> C.Datatype
autoPlay ball paddle score eff = case eff of

  -- new score:
  C.Output (-1) (C.Output 0 (C.Output score' eff')) -> autoPlay ball paddle score' eff'

  -- new tile position
  C.Output x (C.Output _ (C.Output tileId eff'))
    -- paddle
    | tileId == 3 -> autoPlay ball x score eff'
    
    -- ball
    | tileId == 4 -> autoPlay x paddle score eff'
    
    -- rest
    | otherwise -> autoPlay ball paddle score eff'

  -- joystick control:
  C.Input f -> let direction = signum $ ball - paddle
               in
                autoPlay ball paddle score (f direction)
  -- game over:
  C.Halt _ -> score

  _ -> error "invalid output, it should come in triples"
