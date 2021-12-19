module Vector where

data V2 = V2
  { vx :: Int,
    vy :: Int
  }
  deriving (Eq)

data V3 = V3
  { vx3 :: Int,
    vy3 :: Int,
    vz3 :: Int
  }
  deriving (Eq, Ord)

instance Show V3 where
  show (V3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Show V2 where
  show (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

class Euclidean a where
  manhattan :: a -> Int

instance Euclidean V2 where
  manhattan (V2 x y) = abs x + abs y

instance Euclidean V3 where
  manhattan (V3 x y z) = abs x + abs y + abs z

instance Num V2 where
  (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
  (V2 x1 y1) - (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
  (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
  abs (V2 x y) = V2 (abs x) (abs y)
  signum (V2 x y) = V2 (signum x) (signum y)
  fromInteger x = V2 (fromInteger x) (fromInteger x)

instance Num V3 where
  (V3 x1 y1 z1) + (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  (V3 x1 y1 z1) - (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)
  (V3 x1 y1 z1) * (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs (V3 x y z) = V3 (abs x) (abs y) (abs z)
  signum (V3 x y z) = V3 (signum x) (signum y) (signum z)
  fromInteger x = V3 (fromInteger x) (fromInteger x) (fromInteger x)

rotate :: Int -> V2 -> V2
rotate angle (V2 x y) =
  let cos' = round $ cos $ fromIntegral angle / 180 * pi
      sin' = round $ sin $ fromIntegral angle / 180 * pi
   in V2 (x * cos' - y * sin') (x * sin' + y * cos')

rotateX :: Int -> V3 -> V3
rotateX angle (V3 x y z) =
  let cos' = round $ cos $ fromIntegral angle / 180 * pi
      sin' = round $ sin $ fromIntegral angle / 180 * pi
   in V3 x (y * cos' - z * sin') (y * sin' + z * cos')

rotateY :: Int -> V3 -> V3
rotateY angle (V3 x y z) =
  let cos' = round $ cos $ fromIntegral angle / 180 * pi
      sin' = round $ sin $ fromIntegral angle / 180 * pi
   in V3 (x * cos' + z * sin') y (- x * sin' + z * cos')

rotateZ :: Int -> V3 -> V3
rotateZ angle (V3 x y z) =
  let cos' = round $ cos $ fromIntegral angle / 180 * pi
      sin' = round $ sin $ fromIntegral angle / 180 * pi
   in V3 (x * cos' - y * sin') (x * sin' + y * cos') z