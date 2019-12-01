import System.IO

calculateFuel :: Int -> Int
calculateFuel mass = let fuel = (mass `div` 3) - 2 in
    if fuel > 0 then
        fuel + calculateFuel fuel
    else
        0
      
main = do  
    handle <- openFile "../input.txt" ReadMode  
    contents <- hGetContents handle
    let moduleFuels = map (calculateFuel.read) $ lines contents
    putStr $ show $ sum moduleFuels
    hClose handle