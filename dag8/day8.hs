import Data.List
import Data.Char ( digitToInt )


data Tree = Tree {
        height :: Int,
        seen :: Bool,
        visibilityScore :: Int
    }  deriving(Show, Eq)


parseTree :: Char -> Tree
parseTree x = Tree num False (-1)
    where num = digitToInt x

parseRow :: [Char] -> [Tree]
parseRow r = map parseTree r

markSeen :: Tree -> Tree
markSeen t = Tree (height t) True 0

scoreForTree :: Tree -> Int -> Tree
scoreForTree tree score = Tree (height tree) (seen tree) score

seenTrees :: Int -> [Tree] -> [Tree] -> [Tree]
seenTrees prevHigh seen [] = seen
seenTrees prevHigh seen [a] = seen ++ [markSeen a]
seenTrees prevHigh seen (x:xs) =
        if prevHigh < height x
            then seenTrees (height x) (seen ++ [markSeen x]) xs
        else seenTrees prevHigh (seen ++ [x]) xs


visibilityForTree:: [Tree] -> [Tree] -> [Tree]
visibilityForTree seen [] = seen
visibilityForTree seen [a] = seen ++ [scoreForTree a 0]
visibilityForTree  seen (x:xs) =
    let currentH = height x
        visibilityS = min (length xs) (1 + length (takeWhile (\t -> currentH > height t) xs))
        newVisibilityscore = if visibilityScore x == -1
                             then
                                visibilityS
                             else
                                visibilityS * visibilityScore x
        updatedX = scoreForTree x newVisibilityscore
    in
        visibilityForTree (seen ++ [updatedX]) xs


performOnForest :: ([Tree] -> [Tree]) -> [[Tree]] -> [[Tree]]
performOnForest op forest =
    let
        left = map op forest
        top = map op $ transpose left
        bottom = map (op . reverse) top
        right = map (op . reverse) (transpose $ map reverse bottom)
        done = map reverse right
    in
        done


main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let
        partA = performOnForest (seenTrees (-1) []) $ map parseRow ls
        partB = performOnForest (visibilityForTree []) $ map parseRow ls
        in do
            print $ length $ filter (==True) $ concatMap (map seen) partA
            print $ maximum $ concatMap (map visibilityScore) partB


