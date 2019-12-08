import System.IO
import System.Process
import Data.List
import Control.Monad

getInput :: String -> String -> String
getInput phase input = phase ++ "\n" ++ input ++ "\n"

main = do
    let perm = map (map show) $ permutations [0..4]

    let output = map 
            (foldl 
                (\ioin phase -> do
                    input <- ioin
                    let stdin = getInput phase input
                    head.lines <$> readProcess "./amplifier" [] stdin) 
                (return "0")
            )
            perm :: [IO String]
    
    values <- foldr (liftM2 (:)) (return []) output

    print $ foldr max 0 (map read values :: [Int])