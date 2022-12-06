import Data.List

isMarker :: Eq a => Int -> [a] -> Bool
isMarker messagelength candidate = length (nub candidate) == messagelength

lookUntilMarker :: Eq a => Int -> [a] -> [a] -> [a]
lookUntilMarker _messageLength _checked [] = []
lookUntilMarker messageLength checked candidates = 
    let candidate = take messageLength candidates
        hasMarker = isMarker messageLength candidate
    in
        if hasMarker 
            then checked ++ candidate 
            else lookUntilMarker messageLength (checked ++ take 1 candidates) (drop 1 candidates)

main :: IO ()
main = do
    ls <- fmap (head . lines) (readFile "input.txt")
    print $ length $ lookUntilMarker 4 "" ls
    print $ length $ lookUntilMarker 14 "" ls
