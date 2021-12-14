{-# LANGUAGE TupleSections #-}

module Util where

import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (sort)

(<=>) :: Ord a => a -> a -> a -> Bool
x <=> y = \z -> x <= z && z <= y

isJust :: Maybe a -> Bool
isJust = maybe False (const True)

chunks :: Integral c => c -> [a] -> [[a]]
chunks c l =
  let (x, xs) = splitAt (fromIntegral c) l
   in if null xs then [x] else x : chunks c xs

-- https://github.com/quchen/articles/blob/master/loeb-moeb.md
löb :: Functor f => f (f a -> a) -> f a
löb x = go where go = fmap ($ go) x

lIntListToInt :: Int -> [Int] -> Int
lIntListToInt base = foldl (\acc x -> acc * base + x) 0

rIntListToInt :: Int -> [Int] -> Int
rIntListToInt base = foldr (\x acc -> acc * base + x) 0

median :: (Fractional b, Integral a) => [a] -> b
median xs =
  let sorted = sort xs
      l = length sorted
      middle = sorted !! (l `div` 2)
   in if odd l
        then fromIntegral middle
        else fromIntegral ((sorted !! (l `div` 2 - 1)) + middle) / 2

applyN :: Int -> (b -> b) -> b -> b
applyN n f = foldr (.) id (replicate n f)

countAll :: (Hashable k, Num v) => [k] -> M.HashMap k v
countAll xs = M.fromListWith (+) $ map (,1) xs