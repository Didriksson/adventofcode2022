{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import Data.Maybe
import Data.Char

data Command = Cd | Ls | UnExpectedValue String
    deriving (Show)

isCommand ('$':xs) = True
isCommand _ = False


goUpInPath currentPath = reverse . drop 1 . dropWhile (/= '/') $ reverse currentPath

parsePath currentpath path =
    case path of
        ".." -> goUpInPath currentpath
        "/" -> "/"
        _ -> currentpath ++ "/" ++ path

handleCommand [] result currentPath = result
handleCommand (command:rest) result currentPath =
    case (words . tail) command of
                        ("cd":xs) -> handleCommand rest (result) (parsePath currentPath (concat xs))
                        ("ls":xs) -> handleCommand (dropWhile (not . isCommand) rest) (result ++ [(currentPath, (takeWhile (not . isCommand) rest))]) currentPath
                        otherwise -> handleCommand rest result currentPath


toNumberForDirectory content = map read $ filter (/= "") $ map (takeWhile isNumber) content :: [Int]

filesForDirectory d directories = map snd affectedDirectries
    where affectedDirectries = filter (isInfixOf  d . fst) directories

sumForDirectory d directories =
    map toNumberForDirectory $ filesForDirectory d directories

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let filetree = handleCommand ls [] ""
        in do
            print $ sum $ filter (<100000) $ map (snd . ((\d -> ( d, sum . concat $ sumForDirectory d filetree)) . fst)) filetree



