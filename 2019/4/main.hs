range = [197487..673251] :: [Int]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

doubleDigits :: [Int] -> Bool
doubleDigits [] = False
doubleDigits [x] = False 
doubleDigits (x:xs) = x == head xs || doubleDigits xs

monotone :: [Int] -> Bool
monotone [] = True
monotone [x] = True
monotone (x:xs) = x <= head xs && monotone xs

main = do
    let digits = map digs range
    let part1 = filter (\v -> monotone v && doubleDigits v) digits
    putStrLn $ "part1: " ++ show (length part1)