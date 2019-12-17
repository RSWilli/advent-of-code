import InputParser

fttParser :: Parser [Int]
fttParser = map (read .pure) <$> many anySingle

repeatN :: Int -> Int -> [Int]
repeatN n = take n . repeat

basePattern :: [Int]
basePattern = [1, 0, -1, 0]

matrix :: Int -> [[Int]]
matrix l= map (\n -> repeatN n 0 ++ cycle (concatMap (repeatN (n+1)) basePattern)) [0..(l-1)]

step :: [Int] -> [Int]
step l = map ( (`mod` 10) . abs . (foldr ((+) . uncurry (*)) 0) . zip l) $ matrix $ length l

main :: IO()
main = do
  [input] <- getParsedLines 16 fttParser
  print $ concatMap (show) $ take 8 $ last $ take 101 $ iterate step input 