import Data.List
import Data.Char ( digitToInt )

type Coordinate = (Int, Int)

type Rope = [Knot]

data Knot = Knot {
    coord :: Coordinate,
    visited :: [Coordinate]
} deriving(Show)

type Move = Coordinate



addDelta :: Coordinate -> Int -> Int -> Coordinate
addDelta (x,y) dx dy = (x + dx, y + dy)

moveKnot :: Knot -> Coordinate -> Knot
moveKnot knot coord = Knot coord (coord : visited knot)

distanceBetween :: Coordinate -> Coordinate -> Coordinate
distanceBetween (c1x, c1y) (c2x, c2y) = (c2x - c1x, c2y - c1y)

needToMove :: Coordinate -> Bool
needToMove (x, y) = abs x > 1 || abs y > 1

limitDelta :: Int -> Int
limitDelta n
    | n == 0 = 0
    | n > 0 = 1
    | n < 0 = -1


moveTails :: Knot -> Rope -> Rope
moveTails _ [] = []
moveTails knotH (knotT:restOfRope) =  
    let deltaDistance = distanceBetween (coord knotT) (coord knotH)
    in
        if needToMove deltaDistance
        then 
            let movedTail = moveKnot knotT $ addDelta (coord knotT) (limitDelta (fst deltaDistance)) (limitDelta (snd deltaDistance)) 
            in
                movedTail : moveTails movedTail restOfRope
        else knotT : restOfRope

move :: Rope -> Move -> Rope
move (knotH:tails) (dx, dy) =  
    let 
        movedHead = moveKnot knotH $ addDelta (coord knotH) dx dy 
    in
        movedHead : moveTails movedHead tails

toNumber :: [[Char]] -> Int
toNumber input = read $ head input

parseMove :: [Char] -> [Move]
parseMove moveRaw = 
    case words moveRaw of
        ("R":xs) -> map (const (1, 0)) [1..toNumber xs]
        ("L":xs) -> map (const (- 1, 0)) [1..toNumber xs]
        ("U":xs) -> map (const (0, 1)) [1..toNumber xs]
        ("D":xs) -> map (const (0, - 1)) [1..toNumber xs]

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let rope = replicate 10 (Knot (0, 0) [(0, 0)])
        moves = concatMap parseMove ls
        result = foldl move rope moves
        in do
            print $ length $ nub $ visited (result !! 1)
            print $ length $ nub $ visited $ last result