import Control.Monad
import Data.Char
import Data.List
import Data.List.Split (divvy)
import Data.Either
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec

type Coordinate = (Int, Int)

data Board =  Board {
  minX :: Int,
  minY :: Int,
  maxX :: Int,
  maxY :: Int
} deriving(Show)

data Cave = Cave {
  rocks :: [Coordinate],
  sand  :: [Coordinate],
  boundBox :: Board
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

reachedInifinity (x,y) (Board minX minY maxX maxY)
  | x < minX = True
  | y < minY = True
  | x > maxX = True
  | y > maxY = True
  | otherwise = False


createBoundBox paths = Board (minimum xs) (minimum ys) (maximum xs) (maximum ys)
    where
      pathPoints = (500,0) : concat (concat paths)
      xs =  map fst pathPoints
      ys =  map snd pathPoints

expandPaths p = map (\(c1:c2:_) -> expandRange c1 c2) $ divvy 2 1 p

dropSand (Cave rocks sand box) =
   let pos = last takeWhile (\f -> f `elem` occupiedPositions) dropCoordinates
    

  where 
    sandDropPosition = (500,0)
    occupiedPositions = rocks ++ sand
    maxY = maxY box
    dropCoordinates = [(x,y) | x <- [500], y <- [(snd sandDropPosition)..maxY]]

main :: IO ()
main = do
  ls <- readFile "input.txt"
  let pathInput = fromRight [] $ parse pathParser "" ls
  let expandedPaths = map expandPaths pathInput
  let box = createBoundBox expandedPaths
  let initState = Cave (concat $ concat expandedPaths) [] box
  print $ initState