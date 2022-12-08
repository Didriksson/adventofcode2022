import Data.List
import Data.Char ( digitToInt )


data Tree = Tree {
        height :: Int,
        seen :: Bool
    }  deriving(Show, Eq)


parseTree :: Char -> Tree
parseTree x = Tree num False
    where num = digitToInt x

parseRow :: [Char] -> [Tree]
parseRow r = map parseTree r

markSeen :: Tree -> Tree
markSeen t = Tree (height t) True

seenTrees :: Int -> [Tree] -> [Tree] -> [Tree]
seenTrees prevHigh seen [] = seen
seenTrees prevHigh seen [a] = seen ++ [markSeen a]
seenTrees prevHigh seen (x:xs) =
        if prevHigh < height x
            then seenTrees (height x) (seen ++ [markSeen x]) xs
        else seenTrees prevHigh (seen ++ [x]) xs

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let
        left = map (seenTrees (-1) [] . parseRow) ls
        top = map (seenTrees (-1) []) $ transpose left
        bottom = map (seenTrees (-1) [] . reverse) top
        right = map (seenTrees (-1) [] . reverse) (transpose $ map reverse bottom)
        done = map reverse right
        in do
            print $ length $ filter (==True) $ concatMap (map seen) done


