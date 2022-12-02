import qualified Data.Text    as Text
import Data.List

data Shape = Rock | Paper | Scissors
    deriving (Show)

data Result = Win Int | Draw | Loss
    deriving (Show, Eq) 

parseOpponent :: [Char] -> Shape
parseOpponent "A" = Rock
parseOpponent "B" = Paper
parseOpponent "C" = Scissors

parseMyShape :: [Char] -> Shape
parseMyShape "X" = Rock
parseMyShape "Y" = Paper
parseMyShape "Z" = Scissors

parseExpectedResult :: [Char] -> Result
parseExpectedResult "X" = Loss
parseExpectedResult "Y" = Draw
parseExpectedResult "Z" = Win

roundResult :: (Shape, Shape) -> Result
roundResult (Rock, Scissors) = Win
roundResult (Rock, Rock) = Draw
roundResult (Paper, Rock) = Win
roundResult (Paper, Paper) = Draw
roundResult (Scissors, Paper) = Win
roundResult (Scissors, Scissors) = Draw
roundResult (_, _) = Loss

getPointsShape :: Num p => Shape -> p
getPointsShape Rock = 1
getPointsShape Paper = 2
getPointsShape Scissors = 3

getPoints :: Num p => (Shape, Shape) -> p
getPoints (me, opponent) =
    case roundResult (me, opponent) of
        Win -> 6 + getPointsShape me
        Draw -> 3 + getPointsShape me
        Loss -> 0 + getPointsShape me

getShapesForExpectedOutcome :: (Result, Shape) -> (Shape, Shape)
getShapesForExpectedOutcome (r, s) 
    | roundResult(Rock, s) == r = (Rock, s)
    | roundResult(Paper, s) == r = (Paper, s)
    | roundResult(Scissors, s) == r = (Scissors, s)


parsePart1 :: [[Char]] -> (Shape, Shape)
parsePart1 [x,y] = ( parseMyShape y, parseOpponent x)

parsePart2 :: [[Char]] -> (Result, Shape)
parsePart2 [x,y] = ( parseExpectedResult y, parseOpponent x)

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    do
        print $ sum $ map (getPoints . parsePart1 . words) ls
        print $ sum $ map (getPoints . getShapesForExpectedOutcome . parsePart2 . words) ls
        