range = [197487 .. 673251] :: [Int]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

doubleDigits :: [Int] -> Bool
doubleDigits []       = False
doubleDigits [x     ] = False
doubleDigits (x : xs) = x == head xs || doubleDigits xs

monotone :: [Int] -> Bool
monotone []       = True
monotone [x     ] = True
monotone (x : xs) = x <= head xs && monotone xs

exactlyOnePair :: [Int] -> Bool
exactlyOnePair []  = False
exactlyOnePair [x] = False
exactlyOnePair xs =
  let x = head xs
      eq = takeWhile (x ==) xs
      neq = dropWhile (x ==) xs
      in
        length eq == 2 || exactlyOnePair neq

main :: IO ()
main = do
  let digits = map digs range
  let part1 = filter (\v -> monotone v && doubleDigits v) digits
  let part2 = filter (\v -> monotone v && exactlyOnePair v) digits
  putStrLn $ "part1: " ++ show (length part1)
  putStrLn $ "part2: " ++ show (length part2)
