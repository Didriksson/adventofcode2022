import qualified Data.Text    as Text
import Data.List

parseInputRaw :: [[[Char]]] -> [[Char]] -> [[[Char]]]
parseInputRaw parsed [] = parsed
parseInputRaw parsed toParse = parseInputRaw (parsed ++ [takeWhile (/= "") toParse]) (drop 1 (dropWhile (/= "") toParse))


toIntegerList :: [String] -> [Int]
toIntegerList xs = map read xs :: [Int]

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let sortedList = reverse (sort (map (sum . toIntegerList) (parseInputRaw [] ls)))
        in do
            print (head sortedList)
            print (sum $ take 3 sortedList)

