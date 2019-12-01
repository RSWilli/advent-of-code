import System.IO

calculateFuel :: Double -> Int
calculateFuel mass = floor (mass / 3) - 2
      
main = do  
    handle <- openFile "input.txt" ReadMode  
    contents <- hGetContents handle
    let numbers = map (calculateFuel.read) $ lines contents
    putStr $ show $ sum numbers
    hClose handle