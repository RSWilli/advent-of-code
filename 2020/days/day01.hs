import Bench
import qualified Data.Set as S
import InputParser

part1 list =
  let set = S.fromList list
   in product $ filter (`S.member` set) $ map (2020 -) list

part2 list =
  let set = S.fromList list
      first = map (2020 -) list
   in product $ S.fromList $ filter (`S.member` set) $ concatMap (\first -> map (first -) list) first

main = do
  input <- parseInputLines 1 number
  print input
  print (part1 input)
  print (part2 input)
  defaultMain
    [ -- bgroup
      --   "parse"
      --   [ bench "input" $ nfIO (parseInputLines 1) number
      --   ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]