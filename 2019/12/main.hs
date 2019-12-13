import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Map as M
data Vector = Vector {x :: Int, y :: Int, z :: Int}

data Moon = Moon {position :: Vector, velocity = Vector}

vOp :: (Int -> Int -> Int) -> Vector -> Vector -> Vector
vOp f a b = Vector {x = x a `f` x b, y = y a `f` y b, z = z a `f` z b}

vPlus :: Vector -> Vector -> Vector
vPlus = vOp (+)

vMinus :: Vector -> Vector -> Vector
vMinus = vOp (-)

getGravity :: Ord a => (b -> a) -> b -> b -> Int
getGravity f a b = case comparing f a b of
    LT -> -1
    GT -> 1
    EQ -> 0

-- getVelocities :: S.Set Moon -> M.Map Moon Vector
-- getVelocities moons = let compareWithAll v = 
