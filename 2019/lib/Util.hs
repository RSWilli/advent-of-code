module Util 
( module Util ) where

chunks :: Integral c => c -> [a] -> [[a]]
chunks c l =
  let (x, xs) = splitAt (fromIntegral c) l in if null xs then [x] else x : chunks c xs

index :: [a] -> [(Int, a)]
index = zip [0..]