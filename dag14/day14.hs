import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split (divvy)
import Data.Either
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec

type Coordinate = (Int, Int)

data Board =  Board {
  maxX :: Int,
  maxY :: Int
} deriving(Show)

data Cave = Cave {
  rocks :: [Coordinate],
  sand  :: [Coordinate],
  boundBox :: Board,
  caveFloor :: Int
} deriving (Show)

pathParser :: GenParser Char st [[Coordinate]]
pathParser = sepBy path eol
  where
    path = coordinate `Text.Parsec.sepBy` string " -> "
    eol = char '\n'
    coordinate = do
                  x <- many digit
                  char ','
                  y <- many digit
                  return (read x::Int, read y::Int)


rangeFor v1 v2 = if v1 > v2
                 then [v2..v1]
                 else [v1..v2]

expandRange c1 c2 = [ (x,y) | x <- rangeFor (fst c1) (fst c2), y <- rangeFor (snd c1) (snd c2)]

reachedInifinity (x,y) (Board maxX maxY)
  | x > maxX = True
  | y > maxY = True
  | otherwise = False


createBoundBox paths = Board (maximum xs) (maximum ys)
    where
      pathPoints = (500,0) : concat (concat paths)
      xs =  map fst pathPoints
      ys =  map snd pathPoints

expandPaths p = map (\(c1:c2:_) -> expandRange c1 c2) $ divvy 2 1 p

dropSand (Cave rocks sand box caveFloor) sandDropPosition =
  case takeWhile (`notElem` occupiedPositions) dropCoordinates of
    [] -> Nothing
    potential -> Just $ last potential
  where
    boundedY = maxY box
    dropCoordinates = [(x,y) | x <- [fst sandDropPosition], y <- [(snd sandDropPosition)..boundedY]]
    occupiedPositions = rocks ++ sand ++ filter (\c -> snd c == caveFloor) dropCoordinates

isFree occupied potential caveFloor = potential `notElem` occupied && snd potential /= caveFloor
lowerLeft coord = (fst coord - 1, snd coord +1)
lowerRight coord = (fst coord + 1, snd coord +1)

evaluateAndDrop :: Cave -> Coordinate -> Maybe Coordinate
evaluateAndDrop cave dropCoord =
    let
      dropped = dropSand cave dropCoord
      occupiedPositions = rocks cave ++ sand cave
    in
      case dropped of
        Nothing -> Nothing
        Just coord ->
            if isFree occupiedPositions (lowerLeft coord) (caveFloor cave)
              then evaluateAndDrop cave (lowerLeft coord)
            else if isFree occupiedPositions (lowerRight coord) (caveFloor cave)
              then evaluateAndDrop cave (lowerRight coord)
            else
              dropped


-- Drop from 500
-- Do until come to rest
dropUntilInfinity (Cave rockblocks sandblocks box cavefloor) =
    let sandDropPosition = (500,0)
        atRestCoord = evaluateAndDrop (Cave rockblocks sandblocks box cavefloor) sandDropPosition
    in
        case atRestCoord of
          Just coord -> trace("At rest: " ++ show coord ++ " round: " ++ show (length sandblocks)) dropUntilInfinity (Cave rockblocks (coord:sandblocks) box cavefloor)
          Nothing -> Cave rockblocks sandblocks box cavefloor


main :: IO ()
main = do
  ls <- readFile "input.txt"
  let pathInput = fromRight [] $ parse pathParser "" ls
  let expandedPaths = map expandPaths pathInput
  let box = createBoundBox expandedPaths
  let initState = Cave (concat $ concat expandedPaths) [] box 11
  let initStateB = Cave (concat $ concat expandedPaths) [] (Board (maxX box) (maxY box + 2)) (maxY box + 2)
  print $ length $ sand $ dropUntilInfinity initStateB