{-# LANGUAGE DeriveFoldable #-}

module TwoD
  ( TwoD (field, TwoD),
    Pos,
    lookup2D,
    dimensions,
    parseInput2D,
    parseTest2D,
    map2D,
    fold2D,
    ifold2D,
    neighs,
    neighsDiag,
  )
where

import qualified Data.Array as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import InputParser (getInputPath, getTestPath)
import Util (chunks)

type Pos = (Int, Int)

newtype TwoD a = TwoD {field :: A.Array Pos a} deriving (Foldable)

instance Show a => Show (TwoD a) where
  show (TwoD a) = grid
    where
      (_, (height, width)) = A.bounds a
      grid = unlines $ map (unwords . map show) $ chunks (width + 1) $ A.elems a

fileTo2D :: String -> (Char -> a) -> IO (TwoD a)
fileTo2D path cc = do
  content <- map BS.unpack . BS.lines <$> BS.readFile path
  let height = length content
  let width = length $ head content
  return $ TwoD $ A.listArray ((0, 0), (height -1, width - 1)) $ concatMap (map cc) content

parseInput2D :: Int -> (Char -> a) -> IO (TwoD a)
parseInput2D i = fileTo2D (getInputPath i)

parseTest2D :: Int -> Int -> (Char -> a) -> IO (TwoD a)
parseTest2D i j = fileTo2D (getTestPath i j)

lookup2D :: TwoD a -> Pos -> Maybe a
lookup2D poses pos =
  if A.inRange (A.bounds $ field poses) pos
    then Just $ field poses A.! pos
    else Nothing

map2D :: (Pos -> a -> b) -> TwoD a -> TwoD b
map2D fn (TwoD p) =
  let (_, width) = A.bounds p
   in TwoD $ A.array (A.bounds p) [(k, fn k v) | (k, v) <- A.assocs p]

dimensions :: TwoD a -> Pos
dimensions = snd . A.bounds . field

fold2D :: (b -> a -> b) -> b -> TwoD a -> b
fold2D fn e (TwoD p) = foldl fn e p

ifold2D :: (Pos -> b -> a -> b) -> b -> TwoD a -> b
ifold2D fn e (TwoD p) =
  let (_, width) = A.bounds p
   in foldl (\acc (k, v) -> fn k acc v) e $ A.assocs p

neighs :: Pos -> [Pos]
neighs (x, y) = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

neighsDiag :: Pos -> [Pos]
neighsDiag (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]