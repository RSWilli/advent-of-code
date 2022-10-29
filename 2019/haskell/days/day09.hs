import qualified Computer as C
import InputParser

main :: IO ()
main = do
    [machine] <- getParsedLines 9 C.memoryParser
    print $ C.runIntcodeToList machine [1]
    print $ C.runIntcodeToList machine [2]