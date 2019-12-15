module Util 
( module Util ) where

chunks :: Int -> [a] -> [[a]]
chunks c l =
  let (x, xs) = splitAt c l in if null xs then [x] else x : chunks c xs