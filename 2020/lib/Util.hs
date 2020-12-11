module Util where

(<=>) :: Ord a => a -> a -> a -> Bool
x <=> y = \z -> x <= z && z <= y

isJust :: Maybe a -> Bool
isJust = maybe False (const True)

chunks :: Integral c => c -> [a] -> [[a]]
chunks c l =
  let (x, xs) = splitAt (fromIntegral c) l
   in if null xs then [x] else x : chunks c xs