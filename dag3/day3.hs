import qualified Data.Text    as Text
import Data.List
import Data.Maybe

parseCompartsments xs =  splitAt sizeOfSack xs
    where sizeOfSack = ((length xs) `div` 2)

findCommon (f, s) = intersect f s
findCommonIn group = foldr1 (intersect) group

priorityFor c = fromJust $ fmap (+1) (findIndex (==c) priorityList)
    where priorityList = ['a'..'z'] ++ ['A'..'Z']

parseGroups parsed [] = parsed
parseGroups parsed rem = parseGroups (parsed ++ [take 3 rem]) (drop 3 rem)

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    do
        print $ sum $ map (priorityFor . head . nub . findCommon . parseCompartsments) ls     
        print $ sum $ map (priorityFor . head . nub . findCommonIn) (parseGroups [] ls)