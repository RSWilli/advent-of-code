import System.IO
import InputParser

calculateFuel :: Int -> Int
calculateFuel mass = (mass `div` 3) - 2

calculateFuelR :: Int -> Int
calculateFuelR mass = 
    let fuel = calculateFuel mass
        in
    if fuel > 0 then
        fuel + calculateFuel fuel
    else
        0

main :: IO()
main = do  
    masses <- getParsedLines 1 number
    print masses
    print "Part1:"
    print $ sum $ map calculateFuel masses
    print "Part2:"
    print $ sum $ map calculateFuelR masses
    