import Data.Function (fix)
import InputParser
import Data.List (permutations)

import qualified Computer as C

type ListFn = [C.Datatype] -> [C.Datatype]

main :: IO ()
main = do
    [machine] <- getParsedLines 7 C.memoryParser
    print $ part1 $ C.runIntcodeToList machine
    print $ part2 $ C.runIntcodeToList machine

part1 :: ListFn -> C.Datatype
part1 pgm = maximum [head (thrustController pgm p) | p <- permutations [0..4]]

part2 :: ListFn -> C.Datatype
part2 pgm = maximum [last (thrustController pgm p) | p <- permutations [5..9]]

thrustController :: ListFn -> ListFn
thrustController ctrl phases = tieknot [ctrl << p | p <- phases]

tieknot :: [ListFn] -> [C.Datatype]
tieknot fs = fix (composeLR fs << 0)


-- compose all functions together, output of one is input for the next
composeLR :: [a -> a] -> (a -> a)
composeLR = foldl (flip (.)) id


-- feed: 
(<<) :: ListFn -> C.Datatype -> ListFn
(f << x) xs = f (x:xs)