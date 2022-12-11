import Data.List
import Debug.Trace
import Data.Char

data Monkey = Monkey {
    mId :: Int,
    items :: [Item],
    operation :: Int -> Int,
    testThrow :: Int -> Int,
    numberOfInspects :: Int
}

type Item = Int

instance Show Monkey where
    show (Monkey mId items _ _ inspected) = "Monkey: " ++ show mId ++ " holds items: " ++ show items ++ ". Inspected items: " ++ show inspected

instance Eq Monkey where
    (==) (Monkey mId _ _ _ _) (Monkey mId2 _ _ _ _)  = mId == mId2


main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let inputRaw = parseInputUntilBlank [] ls
    let monkeys = map parseMonkey inputRaw
    let partAWorryReducer = (`div` 3)
    let partAResult = performNumberOfLoops 20 monkeys partAWorryReducer
    let partBWorryDevisor = product $ map parseDivisibleBy inputRaw
    let partBWorryReducer = (`mod` partBWorryDevisor)
    let partBResult = performNumberOfLoops 10000 monkeys partBWorryReducer
    print $ product . take 2 . reverse . sort $ map numberOfInspects partAResult
    print $ product . take 2 . reverse . sort $ map numberOfInspects partBResult

addItem :: Monkey -> Item -> Monkey
addItem (Monkey mId items operation testThrow numberOfInspects) item = Monkey mId (items ++ [item]) operation testThrow numberOfInspects

inspectAndReduce :: Monkey -> (Int -> Int) -> [Item]
inspectAndReduce (Monkey _ items operation _ _) reducefunction = map (\i -> reducefunction (operation i)) items

markIinspectedAndThrown :: Monkey -> Monkey
markIinspectedAndThrown (Monkey mId items operation testThrow numberOfInspects) = Monkey mId [] operation testThrow (numberOfInspects + length items)

throwItems :: [Monkey] -> [(Int, Item)] -> [Monkey]
throwItems monkeystate [] = monkeystate
throwItems monkeystate ((toThrowId, item):xs) = 
    throwItems thrownAnItem xs
    where thrownAnItem = map (\m -> if mId m == toThrowId then addItem m item else m) monkeystate

performLoop monkeys monkeyId worryFunction = 
    let 
        monkey = monkeys !! monkeyId
        itemsToThrow = inspectAndReduce monkey worryFunction
        updatedMonkeyState = map (\m -> if m == monkey then markIinspectedAndThrown m else m) monkeys
        throwItemsTo = map (\it -> (testThrow monkey it, it)) itemsToThrow
    in
         throwItems updatedMonkeyState throwItemsTo
    
performNumberOfLoops :: Int -> [Monkey] -> (Int -> Int) -> [Monkey]
performNumberOfLoops 0 state worryFunction = state
performNumberOfLoops rounds state worryFunction = performNumberOfLoops (rounds - 1) updatedState worryFunction
    where updatedState = foldl (\resultstate mid -> performLoop resultstate mid worryFunction) state [0.. (length state -1)]

--- Parse

parseInputUntilBlank :: [[[Char]]] -> [[Char]] -> [[[Char]]]
parseInputUntilBlank parsed [] = parsed
parseInputUntilBlank parsed toParse = parseInputUntilBlank (parsed ++ [takeWhile (/= "") toParse]) (drop 1 (dropWhile (/= "") toParse))
parseMonkeyItems raw = 
    map read $ filter (/= "") $ map (filter isNumber) $ words $ raw !! 1 :: [Int]

parseMonkeyName raw = read numberRaw :: Int
    where numberRaw = filter isNumber $ head raw

parseOperator input = case input of
                        "*" -> (*)
                        "+" -> (+)

parseNumber value = read value :: Int

parseDivisibleBy raw = (read . last . words) $ raw !! 3 :: Int

parseTest raw = \f -> if f `rem` divisibleBy == 0 then throwIfTrue else throwIfFalse 
    where 
        divisibleBy = parseDivisibleBy raw
        throwIfTrue = (read . last . words) $ raw !! 4 :: Int
        throwIfFalse = (read . last . words) $ raw !! 5 :: Int

parseOperation raw = 
    case argumentsAndOperands of
        (operator:"old":"old":_rest) -> \o -> parseOperator operator o o
        (operator:value:"old":_rest) -> parseOperator operator (parseNumber value)
    where argumentsAndOperands = sort $ reverse $ take 3 $ reverse $ words $ raw !! 2 -- Sort will make operands like - and / not work but parsing easier.

parseMonkey :: [[Char]] -> Monkey
parseMonkey rawMonkey = Monkey (parseMonkeyName rawMonkey) (parseMonkeyItems rawMonkey) (parseOperation rawMonkey) (parseTest rawMonkey) 0