import Bench
import Control.Monad (guard, replicateM)
import Control.Monad.State (MonadState (get, put), State, evalState)
import InputParser
import Util (binToInt)

mapHex :: Char -> String
mapHex '0' = "0000"
mapHex '1' = "0001"
mapHex '2' = "0010"
mapHex '3' = "0011"
mapHex '4' = "0100"
mapHex '5' = "0101"
mapHex '6' = "0110"
mapHex '7' = "0111"
mapHex '8' = "1000"
mapHex '9' = "1001"
mapHex 'A' = "1010"
mapHex 'B' = "1011"
mapHex 'C' = "1100"
mapHex 'D' = "1101"
mapHex 'E' = "1110"
mapHex 'F' = "1111"
mapHex _ = error "invalid hex"

hexStreamParser :: Parser String
hexStreamParser = concatMap mapHex <$> many printChar

data Packet = Op Int Int [Packet] | Lit Int Int deriving (Show, Eq)

versionsSum :: Packet -> Int
versionsSum (Lit v _) = v
versionsSum (Op v _ ps) = v + sum (map versionsSum ps)

run :: Packet -> Int
run (Lit _ v) = v
run (Op _ 0 ps) = sum (map run ps)
run (Op _ 1 ps) = product (map run ps)
run (Op _ 2 ps) = minimum (map run ps)
run (Op _ 3 ps) = maximum (map run ps)
run (Op _ 5 [x, y]) = fromEnum $ run x > run y
run (Op _ 6 [x, y]) = fromEnum $ run x < run y
run (Op _ 7 [x, y]) = fromEnum $ run x == run y
run _ = error "invalid type id"

type PacketParser a = State String a

next :: Int -> PacketParser String
next n = do
  s <- get
  let (x, y) = splitAt n s
  put y
  return x

version :: PacketParser Int
version = binToInt <$> next 3

typ :: PacketParser Int
typ = version

literalDigit :: PacketParser (Bool, String)
literalDigit = do
  xxs <- next 5
  let (x : xs) = xxs
  return (x == '0', xs)

literal :: PacketParser Int
literal = binToInt . concat <$> digits
  where
    digits = do
      (isLast, i) <- literalDigit
      if isLast
        then return [i]
        else do
          is <- digits
          return $ i : is

lengthID :: PacketParser Int
lengthID = do
  l <- next 1
  return $ binToInt l

totalLengthParser :: PacketParser Int
totalLengthParser = binToInt <$> next 15

packetCountParser :: PacketParser Int
packetCountParser = binToInt <$> next 11

subPacketsParser :: PacketParser [Packet]
subPacketsParser = do
  i <- lengthID
  case i of
    0 -> do
      len <- totalLengthParser
      parseNBits len parsePacket
    1 -> do
      c <- packetCountParser
      replicateM c parsePacket
    _ -> error "invalid lengthID"

parseNBits :: Int -> PacketParser a -> PacketParser [a]
parseNBits n p = do
  inp <- get
  v <- p
  inp' <- get
  let diff = length inp - length inp'
  if diff < n
    then (v :) <$> parseNBits (n - diff) p
    else return [v]

parsePacket :: PacketParser Packet
parsePacket = do
  v <- version
  t <- typ
  case t of
    4 -> Lit v <$> literal
    _ -> Op v t <$> subPacketsParser

parse :: String -> Packet
parse = evalState parsePacket

part1 :: String -> Int
part1 = versionsSum . parse

part2 :: String -> Int
part2 = run . parse

main :: IO ()
main = do
  test11
  test12
  test13
  test14

  test21

  input <- parseInput 16 hexStreamParser

  print (part1 input)

  print (part2 input)

  defaultMain
    [ bgroup
        "parse"
        [ bench "input" $ whnfIO (parseInput 16 hexStreamParser)
        ],
      bgroup
        "run"
        [ bench "part1" $ whnf part1 input,
          bench "part2" $ whnf part2 input
        ]
    ]

test11 :: IO ()
test11 = do
  input <- parseTest 16 1 hexStreamParser
  print $ parse input
  guard $ part1 input == 16
  print ("ok" :: String)

test12 :: IO ()
test12 = do
  input <- parseTest 16 2 hexStreamParser
  print $ parse input
  guard $ part1 input == 12
  print ("ok" :: String)

test13 :: IO ()
test13 = do
  input <- parseTest 16 3 hexStreamParser
  print $ parse input
  guard $ part1 input == 23
  print ("ok" :: String)

test14 :: IO ()
test14 = do
  input <- parseTest 16 4 hexStreamParser
  print $ parse input
  guard $ part1 input == 31
  print ("ok" :: String)

test21 :: IO ()
test21 = do
  input <- parseTest 16 5 hexStreamParser
  print $ parse input
  guard $ part2 input == 1
  print ("ok" :: String)