import Data.List
import Debug.Trace
import Data.Char(ord)
import Data.Maybe

data Node = Node {
    currentPosition :: (Int, Int),
    visited :: [(Int,Int)]
} deriving(Show)

type Height = Int
goal = 'E'

type Grid = [[Char]]

checklocationChar grid (x, y) = (grid !! y) !! x


checkLocationHeight :: Grid -> (Int, Int) -> Int
checkLocationHeight grid (x, y) = case location of
                                'E' -> ord 'z'
                                'S' -> ord 'a'
                                otherwise -> ord otherwise
    where location = checklocationChar grid (x,y)

findStartPosition :: Grid -> (Int,Int)
findStartPosition grid =
    let y = length $ takeWhile (\f -> 'S' `notElem` f) grid
        x = length $ takeWhile (/= 'S') $ grid !! y
    in
        (x,y)

isChickenDinner :: Grid -> (Int, Int) -> Bool
isChickenDinner grid position = checklocationChar grid position == goal

inBounds :: Grid -> (Int,Int) -> Bool
inBounds grid (x, y) =
    x >= 0 && x < boundX && y >= 0 && y < boundY
    where boundY = length grid
          boundX = length $ head grid

getPotentialMoves :: Grid -> Node -> [Node]
getPotentialMoves grid (Node (x,y) visited) =
    let potential = filter (inBounds grid) [(x - 1, y), (x, y-1), (x+1, y), (x, y+1)]
        notVisited = filter (`notElem` visited) potential
        withinHeight = filter (\p -> checkLocationHeight grid (x,y) + 1 >= checkLocationHeight grid p) notVisited
        isGoalinList = find (isChickenDinner grid) withinHeight
    in
        case isGoalinList of
            Just g -> [Node g visited]
            Nothing -> map (\p -> Node p $ visited ++ [p]) withinHeight

reachedEnd :: Grid -> Node -> Bool
reachedEnd grid (Node current visitednodes) =
    isChickenDinner grid current || length visitednodes >= gridSize
    where gridSize = length grid * length (head grid)

loop :: Grid -> [Node] -> Int
loop grid nodes =
    let
        foundGoal = find (reachedEnd grid) nodes
        potential =  concatMap (getPotentialMoves grid) nodes
    in
        case foundGoal of
            Just v -> length (visited v)
            Nothing -> loop grid potential

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let
        startPosition = findStartPosition ls
        initNode = Node startPosition [startPosition]
        result = loop ls [initNode]
    print $ result