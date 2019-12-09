import Data.List.Split
import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromMaybe) 

sequences c l = let (x, xs) = splitAt c l in if null xs then [x] else (x : sequences c xs)

parse :: String -> [String]
parse str = tail $ splitOn "" str

createLayers :: [String] -> Int -> Int -> [[[String]]]
createLayers list height width = map (sequences height) $ sequences (width*height) list

combineLayers :: [[[String]]] -> [M.Map String Int]
combineLayers ll = map (M.fromList . map (\v -> (head v, length v))) $ map (group.sort.concat) ll

getLayerWithMinZeros :: [M.Map String Int] -> M.Map String Int
getLayerWithMinZeros list = snd 
  $ foldl 
    (\(min, minmap) map -> 
      let zerocount = fromMaybe (10000) (M.lookup "0" map)
          in
            case compare min zerocount of
              GT -> (zerocount, map)
              LT -> (min, minmap)
              EQ -> (zerocount, map)
    ) 
    (10000, undefined) 
    list

type Layermap = M.Map (Int, Int, Int) String
type Imagemap = M.Map (Int, Int) String

getPositionMap :: [[[String]]] -> Layermap
getPositionMap layers = M.fromList kv
  where
        index l = zip [0..] l
        kv = concatMap (\(i, layer) -> concatMap 
            (\(x, row) ->
              map (\(y, elem) ->
                ((i,x,y), elem) :: ((Int, Int, Int), String)
              )
              $ index row
            )
            $ index layer 
          )
          $ index layers

createImage :: Layermap -> Imagemap
createImage map = M.foldrWithKey
                    (\(_,x,y) v map -> case v of 
                      "0" -> M.insert (x,y) " " map
                      "1" -> M.insert (x,y) "█" map
                      "2" -> map
                    )
                    M.empty
                    map

main = do
  handle <- openFile "./input.txt" ReadMode  
  contents <- hGetContents handle
  let layers = createLayers (parse contents) 25 6
  -- print layers
  let combinedlayers = combineLayers $ layers
  let minzeros = getLayerWithMinZeros $ combinedlayers
      ones = fromMaybe (0) (M.lookup "1" minzeros)
      twos = fromMaybe (0) (M.lookup "2" minzeros)

  let positionmap = getPositionMap layers

  -- print positionmap

  -- print $ contents == M.foldl (++) "" positionmap -- check if input == parsed

  print "part1:"
  print $ ones * twos
  
  print "part2:"
  let image = createImage positionmap
  -- print image
  putStr $ unlines $ map concat $ sequences 25 $ map snd $ M.toList $ image