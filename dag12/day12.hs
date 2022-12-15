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

findAllLowestPointPositions :: Grid -> [(Int,Int)]
findAllLowestPointPositions grid = map snd $ filter (\c -> fst c == 'a') $ map (\c -> (checklocationChar grid c, c)) allcords
    where allcords = [ (x,y) | x <- [0..length (head grid)-1], y <- [0..length grid -1]]


isChickenDinner :: Grid -> (Int, Int) -> Bool
isChickenDinner grid position = checklocationChar grid position == goal

inBounds :: Grid -> (Int,Int) -> Bool
inBounds grid (x, y) =
    x >= 0 && x < boundX && y >= 0 && y < boundY
    where boundY = length grid
          boundX = length $ head grid

getPotentialMoves :: Grid -> [(Int, Int)] -> Node -> [Node]
getPotentialMoves grid visited (Node (x,y) _)=
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
        visitedpositions = concatMap visited nodes
        foundGoal = find (reachedEnd grid) nodes
        potential = concatMap (\n -> getPotentialMoves grid (visited n) n) nodes
    in
        case foundGoal of
            Just v -> length (visited v)
            Nothing -> loop grid $ nubBy (\n1 n2 -> currentPosition n1 == currentPosition n2) potential

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let
        startPosition = findStartPosition ls
        initNodeA = Node startPosition [startPosition]
        initNodesB = map (\p -> Node p [p]) $ findAllLowestPointPositions ls
        result = loop ls initNodesB
    print $ result