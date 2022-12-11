import Data.List
import Debug.Trace

type CycleTime = Int
data Instruction = Addx CycleTime Int | Noop CycleTime
    deriving (Show)

data StackEntry = StackEntry {
    instruction::Instruction,
    ttl :: Int }
    deriving (Show)

data Program = Program {
    x :: Int,
    running :: [StackEntry],
    instructions :: [Instruction],
    cycle :: Int
} deriving (Show)


getCycleTime:: Instruction -> Int
getCycleTime (Addx c _) = c
getCycleTime (Noop c) = c

instructionsReadyToExecute :: [StackEntry] -> [StackEntry]
instructionsReadyToExecute stack = filter (\(StackEntry _ ttl) -> ttl == 0) stack

instructionsNotReadyToExecute :: [StackEntry] -> [StackEntry]
instructionsNotReadyToExecute stack = filter (\(StackEntry _ ttl) -> ttl /= 0) stack

toNumber :: [Char] -> Int
toNumber n = read n :: Int

parseInstruction :: [Char] -> Instruction
parseInstruction xs = 
    case words xs of
        ("noop":_) -> Noop 1
        ("addx":value) -> Addx 2 $ toNumber (head value) 

execute :: Program -> Instruction -> Program
execute (Program x stack instructions cycle) (Addx _ v) = Program (x + v) stack instructions cycle
execute p _ = p

performCycle :: Program -> Program
performCycle (Program x [] [] cycle) = Program x [] [] cycle
performCycle (Program x [] (nextInstruction:remInstructions) cycle) = performCycle $ Program x [StackEntry nextInstruction (getCycleTime nextInstruction)] remInstructions cycle
performCycle (Program x stack instructions cycle) = 
    let 
        ttlAdjustedStack = map (\(StackEntry i ttl) -> StackEntry i (ttl-1)) stack
        stackEntriesToExecute = instructionsReadyToExecute ttlAdjustedStack
        remainingStackEntries = instructionsNotReadyToExecute ttlAdjustedStack
        newProgram = Program x remainingStackEntries instructions (cycle + 1)
    in
        foldl execute newProgram $ map instruction stackEntriesToExecute


runAllCycles :: Program -> [Int] -> [Int]
runAllCycles (Program x [] [] cycle) valuesPerCycle = valuesPerCycle
runAllCycles program valuesPerCycle =
    let 
        updatesCycleValues = (valuesPerCycle ++ [x program])
        performedCycle = performCycle program
    in
        runAllCycles performedCycle updatesCycleValues
    

drawPixel:: (Int, Int) -> Char 
drawPixel (cycle,sprite) = 
    if  position `elem` spriteRange
    then '#'
    else '.'
    where   position = cycle - 1
            spriteRange = [sprite-1..sprite+1]    

getCRTOutput :: [Int] -> [[Char]]
getCRTOutput [] = [""]
getCRTOutput toParse = 
    map drawPixel (take 40 $ zip [1..] toParse) : getCRTOutput (drop 40 toParse)


main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let program = Program 1 [] (map parseInstruction ls) 0
    let result = runAllCycles program []
    print $ sum $ map (\p -> fst p * snd p) $ filter (\f -> fst f `elem` [20, 60, 100, 140, 180, 220]) $ zip [1..] result
    mapM_ print $ getCRTOutput result