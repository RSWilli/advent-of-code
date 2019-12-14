import qualified Computer as C
import System.IO
import qualified Data.Map as M

type Field = M.Map (Integer, Integer) Integer

sequences :: Integer -> [a] -> [[a]]
sequences c l =
  let (x, xs) = splitAt (fromIntegral c) l in if null xs then [x] else x : sequences c xs

main :: IO ()
main = do
  h    <- openFile "./13/input.txt" ReadMode
  code <- hGetContents h
  let program   = C.parseIntcodeProgram code
  part1 program
  part2 program

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
printField field = unlines $ sequences (getWidth field) $ concatMap (replaceTileID.snd) $ M.toList field


part1 :: C.Memory -> IO ()
part1 program = do
  let triples = sequences 3 $ C.runIntcodeToList program []
  let field = foldl (\f [x,y,tileId] -> M.insert (y,x) tileId f) M.empty triples
  print $ length $ filter ((2==).snd) $ M.toList field



part2 :: C.Memory -> IO  ()
part2 program = do
  let programWithJoystick = M.insert 0 2 program
  let triples = sequences 3 $ C.runIntcodeToList program []
  let field = foldl (\f [x,y,tileId] -> M.insert (y,x) tileId f) M.empty triples
  putStr $ printField field

  let score = autoPlay 0 0 0 (C.run $ C.machine programWithJoystick)

  print score



autoPlay :: Integer -> Integer -> Integer -> C.Effect -> Integer
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
  C.Input f -> let direction = sign $ ball - paddle
               in
                autoPlay ball paddle score (f direction)
  -- game over:
  C.Halt _ -> score

  _ -> error "invalid output, it should come in triples"

sign :: (Ord a, Num a, Num p) => a -> p
sign x = case compare x 0 of
  EQ -> 0
  LT -> -1
  GT -> 1