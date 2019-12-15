{-# Language OverloadedStrings #-}
import           Data.Ord                       ( comparing )
import qualified Data.Map                      as M
import InputParser
import Util

data Vector = Vector {x :: Int, y :: Int, z :: Int}

instance Show Vector where
  show v =
    "<x=" ++ show (x v) ++ ", y=" ++ show (y v) ++ ", z=" ++ show (z v) ++ ">"

newVector :: Int -> Int -> Int -> Vector
newVector x y z = Vector { x = x, y = y, z = z }

sumComponents :: Vector -> Int
sumComponents v = abs (x v) + abs (y v) + abs (z v)

data Moon = Moon {position :: Vector, velocity :: Vector}

instance Show Moon where
  show v = "pos=" ++ show (position v) ++ ", vel=" ++ show (velocity v)

newMoon :: Int -> Int -> Int -> Moon
newMoon x y z = Moon { position = newVector x y z, velocity = newVector 0 0 0 }

getMoonEnergy :: Moon -> Int
getMoonEnergy m =
  let pot = sumComponents $ position m
      kin = sumComponents $ velocity m
  in
    pot * kin

newtype Simulation = Simulation { getSim :: M.Map Int Moon }

newSim :: M.Map Int Moon -> Simulation
newSim m = Simulation { getSim = m }

getSimEnergy :: Simulation -> Int
getSimEnergy sim = M.foldr (\moon en -> en + getMoonEnergy moon) 0 $ getSim sim

instance Show Simulation where
  show sim = unlines $ map (show . snd) $ M.toList $ getSim sim

vOp :: (Int -> Int -> Int) -> Vector -> Vector -> Vector
vOp f a b = Vector { x = x a `f` x b, y = y a `f` y b, z = z a `f` z b }

vPlus :: Vector -> Vector -> Vector
vPlus = vOp (+)

-- vMinus :: Vector -> Vector -> Vector
-- vMinus = vOp (-)



-- end of data types declaration




getGravity :: Ord a => (b -> a) -> b -> b -> Int
getGravity f a b = case comparing f a b of
  LT -> -1
  GT -> 1
  EQ -> 0

applyVelocity :: Vector -> Moon -> Moon
applyVelocity vel moon =
  let cpos    = position moon
      cvel    = velocity moon
      newvel = vPlus vel cvel
      newpos = vPlus newvel cpos
  in  Moon { position = newpos, velocity = newvel }

calculateVelocity :: Int -> Simulation -> Vector
calculateVelocity index sim =
  let moons      = getSim sim
      current    = moons M.! index
      currentVel = velocity current
      currentPos = position current
  in  M.foldr
        (\moon v ->
          let pos = position moon
              diffV = Vector { x = getGravity x pos currentPos
                             , y = getGravity y pos currentPos
                             , z = getGravity z pos currentPos
                             }
          in  vPlus diffV v
        )
        (newVector 0 0 0)
        (M.delete index moons)

simulateStep :: Simulation -> Simulation
simulateStep moons =
  let diffs = M.mapWithKey
                (\k _ -> calculateVelocity k moons)
                $ getSim moons
  in 
    newSim
      $ M.mapWithKey
          (\k moon ->
            let diffV = diffs M.! k in applyVelocity diffV moon
          )
          $ getSim moons

moonParser :: Parser Moon
moonParser = newMoon <$> ("<x=" *> number) <*> (", " *> "y=" *> number) <*> (", " *> "z=" *> number <* ">")

main :: IO ()
main = do
  moons <- getParsedLines 12 moonParser
  let simulation = newSim $ M.fromList $ index moons
  -- putStr $ unlines $ map (\(step, sim) -> "After " ++ show step ++ " steps:\n" ++ show sim ) $ zip [0..] $ take 11 $ iterate simulateStep simulation
  let thousendSteps = last $ take 1001 $ iterate simulateStep simulation
  print thousendSteps
  print $ getSimEnergy thousendSteps
