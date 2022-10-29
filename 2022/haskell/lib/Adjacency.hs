module Adjacency where

import qualified Data.Matrix as M

nthSuccessorMatrix :: Int -> Integer -> M.Matrix Integer -> M.Matrix Integer
nthSuccessorMatrix size n m = go n
  where
    go 0 = M.identity size
    go 1 = m
    go n | odd n = let n' = go ((n - 1) `div` 2) in M.multStd m $ M.multStd n' n'
    go n = let n' = go (n `div` 2) in M.multStd n' n'