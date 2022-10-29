module Search where

import qualified Data.Map as M
import qualified Data.Set as S
import PQueue (PQueue (..))
import qualified PQueue as Q

dijkstra ::
  (Ord a) =>
  -- | successor function
  (a -> [a]) ->
  -- | cost function between to nodes
  (a -> a -> Int) ->
  -- | start node
  a ->
  -- | end node
  a ->
  Maybe Int
dijkstra suc cost s e = go (Q.singleton 0 s) S.empty (M.singleton s 0)
  where
    go q done dist = case q of
      Empty -> dist M.!? e
      x :<| q'
        | x == e -> dist M.!? e
        | S.member x done -> go q' done dist
        | otherwise ->
          let next = filter (`S.notMember` done) $ suc x
              dx = dist M.! x
              done' = S.insert x done
              d y =
                -- min { dist y, dist x + cost x y }
                maybe
                  (dx + cost x y)
                  (min (dx + cost x y))
                  $ dist M.!? y

              (q'', dist') =
                foldr
                  ( \y (q, dist) ->
                      ( Q.insert (d y) y q,
                        M.insert y (d y) dist
                      )
                  )
                  (q', dist)
                  next
           in go q'' done' dist'