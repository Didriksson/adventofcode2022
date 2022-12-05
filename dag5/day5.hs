import Data.List
import Data.Char
import Data.List.Split
import Data.Map (Map, fromList, findWithDefault, adjust)
import qualified Data.Map as Map

data Crane = CrateMover9000 | CrateMover9001
    deriving(Eq)

parseStackRowRaw :: [[Char]] -> [Char] -> [[Char]]
parseStackRowRaw parsed [] = parsed
parseStackRowRaw parsed toParse = parseStackRowRaw (parsed ++ [filter isAlpha (take 3 toParse)]) (drop 4 toParse)

parseStacks xs =  Map.fromList (zip [1..] $ map concat $ transpose stackRows)
    where stackRows = map (parseStackRowRaw []) xs

parseInstructions xs = 
    let splitInstruction = map (splitOneOf "move,from,to") $ drop 1 xs
        rawInstructions =  map (filter (/= "") . map (filter isNumber)) splitInstruction
    in
        map (map read) rawInstructions :: [[Int]]

parseInput xs = (parseStacks stacksinput, parseInstructions instructionsinput)
    where (stacksinput, instructionsinput) = break (=="") xs

handleInstructions _crane stacks [] = stacks
handleInstructions crane stacks ([nrOfStacks, from, to]:remInstructions) = 
    let 
        fullstack = take nrOfStacks $ Map.findWithDefault "0" from stacks
        takenStacks = if crane == CrateMover9000 then reverse fullstack else fullstack
        droppedFromStack = Map.adjust (drop nrOfStacks) from stacks
        updatedStacks = Map.adjust (takenStacks ++) to droppedFromStack
    in 
        handleInstructions crane updatedStacks remInstructions 


calculateWith crane (stacks, instructions) = 
    reverse $ Map.foldr (\a acc -> acc ++ take 1 a) "" $ handleInstructions crane stacks instructions

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let input = parseInput ls
        in do
            print $ calculateWith CrateMover9000 input
            print $ calculateWith CrateMover9001 input
