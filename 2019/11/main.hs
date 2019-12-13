import qualified Computer                      as C
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           System.IO

type Position = (Int, Int)

type Hull = M.Map Position Color

type ListFn = [Int] -> [Int]

data Color = Black | White

data Turn = LEFT | RIGHT

data Direction = U | R | D | L

data Robot = Robot {
    position :: Position,
    direction :: Direction
}

data Hullpainter = Hullpainter {
    robot :: Robot,
    hull :: Hull,
    program :: C.Effect,
    color :: Color
}

newRobot :: Robot
newRobot = Robot { position = (0, 0), direction = U }

newHullpainter :: C.Effect -> Color -> Hullpainter
newHullpainter pgm startcolor = Hullpainter { robot   = newRobot
                                            , hull    = M.empty
                                            , program = pgm
                                            , color   = startcolor
                                            }

paint :: Position -> Color -> Hull -> Hull
paint = M.insert

turn :: Turn -> Direction -> Direction
turn LEFT  U = L
turn LEFT  L = D
turn LEFT  D = R
turn LEFT  R = U

turn RIGHT U = R
turn RIGHT L = U
turn RIGHT D = L
turn RIGHT R = D

step :: Position -> Direction -> Position
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)

step (x, y) U = (x, y + 1)
step (x, y) D = (x, y - 1)

getColor :: Position -> Hull -> Color
getColor pos hull = fromMaybe Black (M.lookup pos hull)

fromColorCode :: Integer -> Color
fromColorCode i = case i of
  0 -> Black
  1 -> White
  _ -> error "unknown color"

toColorCode :: Color -> Integer
toColorCode c = case c of
  Black -> 0
  White -> 1

fromTurnCode :: Integer -> Turn
fromTurnCode i = case i of
  0 -> LEFT
  1 -> RIGHT
  _ -> error "unknown turn code"

colorToChar :: Color -> String
colorToChar Black = "."
colorToChar White = "█"

paintHull :: Hullpainter -> Hullpainter
paintHull hp =
  let effect = program hp
      rbt    = robot hp
      pos    = position rbt
      dir    = direction rbt
      h      = hull hp
      currentcolor = color hp
  in  case effect of
        C.Halt  _     -> hp
        C.Input input -> paintHull
          $ hp { program = input $ toColorCode currentcolor }
        C.Output colorcode (C.Output turncode effect') -> paintHull $ hp
          { hull    = hull'
          , robot   = rbt { position = pos', direction = dir' }
          , program = effect'
          , color = currentcolor'
          }
         where
          hull' = paint pos (fromColorCode colorcode) h
          dir'  = turn (fromTurnCode turncode) dir
          pos'  = step pos dir'
          currentcolor' = getColor pos' hull'
        C.Output _ _ -> error "expected second output"

getBoundingBox :: [Position] -> (Position, Position)
getBoundingBox xs = foldl
  (\((minx, miny), (maxx, maxy)) (x, y) ->
    ((min minx x, min miny y), (max maxx x, max maxy y))
  )
  (head xs, head xs)
  xs

generateImage :: Hull -> String
generateImage hull =
  let ((minx, miny),(maxx, maxy)) = getBoundingBox $ M.keys hull
      pixels       = [ (x, y) | y <- [miny .. maxy], x <- [minx .. maxx] ]

      color pix = colorToChar $ fromMaybe Black (M.lookup pix hull)
  in  unlines $ reverse $ sequences (maxx-minx + 1) $ foldl (\str pix -> str ++ color pix) "" pixels

main = do
  h    <- openFile "./input.txt" ReadMode
  code <- hGetContents h
  let program   = C.parseIntcodeProgram code
  let p1Painter = newHullpainter (C.run $ C.machine program) Black
  let part1     = paintHull p1Painter

  let p2Painter = newHullpainter (C.run $ C.machine program) White
  let part2     = paintHull p2Painter

  print $ M.size $ hull part1

  print $ getBoundingBox $ M.keys $ hull part2

  let image = generateImage $ hull part2

  print $ length image

  putStrLn image


sequences c l =
  let (x, xs) = splitAt c l in if null xs then [x] else x : sequences c xs
