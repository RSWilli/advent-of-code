module Util
  ( module Util
  )
where

import qualified Data.Map                      as M

chunks :: Integral c => c -> [a] -> [[a]]
chunks c l =
  let (x, xs) = splitAt (fromIntegral c) l
  in  if null xs then [x] else x : chunks c xs

index :: [a] -> [(Int, a)]
index = zip [0 ..]

twoDArrayToMap :: [[a]] -> M.Map (Int, Int) a
twoDArrayToMap l =
  M.fromList [ ((x, y), d) | (y, row) <- index l, (x, d) <- index row ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
