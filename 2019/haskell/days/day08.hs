{-# Language OverloadedStrings #-}
import           InputParser
import Data.List (group, sort, minimumBy)
import Control.Applicative ((<|>),many)
import Util
import qualified Data.Map                      as M

parsePixel :: Parser String
parsePixel = "0" <|> "1" <|> "2"

createLayers :: [String] -> Int -> Int -> [[[String]]]
createLayers list height width =
  map (chunks height) $ chunks (width * height) list

combineLayers :: [[[String]]] -> [M.Map String Int]
combineLayers = map (foldr (\color m -> M.insertWith (+) color 1 m) M.empty . concat)

getLayerWithMinZeros :: [M.Map String Int] -> M.Map String Int
getLayerWithMinZeros = minimumBy (\m1 m2 -> compare (M.findWithDefault 0 "0" m1) (M.findWithDefault 0 "0" m2))

type Layermap = M.Map (Int, Int, Int) String
type Imagemap = M.Map (Int, Int) String

getPositionMap :: [[[String]]] -> Layermap
getPositionMap layers = M.fromList kv
 where
  index = zip [0 ..]
  kv =
    concatMap
        (\(i, layer) ->
          concatMap
              (\(x, row) ->
                map
                    (\(y, elem) ->
                      ((i, x, y), elem) :: ((Int, Int, Int), String)
                    )
                  $ index row
              )
            $ index layer
        )
      $ index layers

createImage :: Layermap -> Imagemap
createImage = M.foldrWithKey
  (\(_, x, y) v map -> case v of
    "0" -> M.insert (x, y) " " map
    "1" -> M.insert (x, y) "█" map
    "2" -> map
  )
  M.empty

main :: IO ()
main = do
  imagedata <- getParsedInput 8 (many parsePixel)
  let layers         = createLayers imagedata 25 6
  print layers
  let combinedlayers = combineLayers layers
  let minzeros = getLayerWithMinZeros combinedlayers
      ones     = M.findWithDefault 0 "1" minzeros
      twos     = M.findWithDefault 0 "2" minzeros

  print minzeros
  let positionmap = getPositionMap layers

  putStrLn "part1:"
  print $ ones * twos

  putStrLn "part2:"
  let image = createImage positionmap
  -- print image
  putStr $ unlines $ map concat $ chunks 25 $ map snd $ M.toList image
