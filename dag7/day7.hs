{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import Data.Maybe
import Data.Char

data Command = Cd | Ls | UnExpectedValue String
    deriving (Show)

isCommand :: [Char] -> Bool
isCommand ('$':xs) = True
isCommand _ = False


goUpInPath :: [Char] -> [Char]
goUpInPath currentPath = reverse . drop 1 . dropWhile (/= '/') $ reverse currentPath

parsePath :: [Char] -> [Char] -> [Char]
parsePath currentpath path =
    case path of
        ".." -> goUpInPath currentpath
        "/" -> "/"
        _ -> currentpath ++ "/" ++ path

handleCommand :: [[Char]] -> [([Char], [[Char]])] -> [Char] -> [([Char], [[Char]])]
handleCommand [] result currentPath = result
handleCommand (command:rest) result currentPath =
    case (words . tail) command of
                        ("cd":xs) -> handleCommand rest result (parsePath currentPath (concat xs))
                        ("ls":xs) -> handleCommand (dropWhile (not . isCommand) rest) (result ++ [(currentPath, (takeWhile (not . isCommand) rest))]) currentPath
                        _ -> handleCommand rest result currentPath


toNumberForDirectory :: [[Char]] -> [Int]
toNumberForDirectory content = map read $ filter (/= "") $ map (takeWhile isNumber) content :: [Int]

filesForDirectory :: Eq a => [a] -> [([a], b)] -> [b]
filesForDirectory d directories = map snd affectedDirectries
    where affectedDirectries = filter (isInfixOf  d . fst) directories

sumForDirectory :: Eq a => [a] -> [([a], [[Char]])] -> [[Int]]
sumForDirectory d directories =
    map toNumberForDirectory $ filesForDirectory d directories

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let filetree = handleCommand ls [] ""
        directoryTree = map (snd . ((\d -> ( d, sum . concat $ sumForDirectory d filetree)) . fst)) filetree
        discspace = 70000000
        unused = 70000000 - maximum directoryTree
        requiredUnused = 30000000
        in do
            print $ sum $ filter (<100000) directoryTree
            print $ minimum (filter (\ s -> unused + s > requiredUnused) directoryTree)



