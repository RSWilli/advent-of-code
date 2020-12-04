import Bench
import Control.Monad (guard)
import qualified Data.Set as S
import InputParser

part1 list =
  let set = S.fromList list
   in product $ filter (`S.member` set) $ map (2020 -) list

part2 list =
  let set = S.fromList list
      indexed = zip [1 ..] list
   in product $ do
        (i, x) <- indexed
        (j, y) <- indexed
        guard $ i <= j
        let v = 2020 - x - y
        guard $ S.member v set
        return v

main = do
  input <- parseInputLines 1 number
  print input
  print (part1 input)
  print (part2 input)
  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInputLines 1 number)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]