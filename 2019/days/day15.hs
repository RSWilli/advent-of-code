import qualified Computer as C
import qualified Data.Dequeue as Q
import qualified Data.Set as S
import qualified Data.Map as M

--                  output     state        x           y
type Position = (C.Datatype, C.Effect, C.Datatype, C.Datatype)

(|>) :: [a] -> Q.BankersDequeue a -> Q.BankersDequeue a
list |> q = foldl Q.pushFront Q.empty list

walk :: Position -> C.Datatype -> Position
walk (o, e, x, y) dir = case dir of
  1 -> (o, e, x, y + 1) -- N
  2 -> (o, e, x, y - 1) -- S
  3 -> (o, e, x - 1, y) -- W
  4 -> (o, e, x + 1, y) -- E

expand :: Position -> C.Effect -> [(Position, C.Effect)]
expand pos eff = case eff of
  C.Input f -> let results = map (\d -> (walk pos d, f d)) [0..3]
               in
                filter  
                  (\(_, eff') -> case eff' of
                    C.Output v _ -> v == 0
                    _ -> error "unknown state"
                  )
                  results
                   
  _ -> error "unknown state"

bfs start robot = bfs' (Q.fromList [(start, robot)]) S.empty M.empty

bfs' :: Q.BankersDequeue (Position, C.Effect) -> S.Set Position -> M.Map Position [Position] -> M.Map Position [Position]
bfs' q vis m = 
  case Q.popBack q of
    Nothing -> m
    Just ((current, eff), q') -> let ns = expand current eff
                                     neigh = filter (\(pos, _) -> not $ S.member pos vis) ns
                                     q'' = neigh |> q'
                                     m' = M.insert current (map fst neigh) m
                                     vis' = foldr (S.insert . fst) vis neigh
                                 in
                                   bfs' q'' vis' m'

main :: IO ()
main = do
  print "hw"
  print "hw2"