import qualified Data.Text    as Text
import Data.List
import Data.Maybe

splitOn x ls = (fst splitted, drop 1 (snd splitted))
    where splitted = break x ls 

toRange r = [read f..read t] :: [Integer]
    where (f, t) = splitOn (=='-') r

rangesForSectionAssignments r = (toRange (fst r), toRange (snd r))

contains f (l, r) = (f (\x -> elem x l) r) ||  (f (\x -> elem x r) l)

checkFor f ranges = length $ filter (contains f) ranges

main :: IO ()
main = do
    ls <- fmap lines (readFile "input.txt")
    let ranges = map (rangesForSectionAssignments . (splitOn (==','))) ls
        in do
            print $ (checkFor all) ranges
            print $ (checkFor any) ranges