module Util where

(<=>) :: Ord a => a -> a -> a -> Bool
x <=> y = \z -> x <= z && z <= y

isJust :: Maybe a -> Bool
isJust = maybe False (const True)