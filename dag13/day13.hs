import Control.Monad
import Data.Char
import Data.Either (fromRight)
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

data Packet = List [Packet] | Value Int
  deriving (Show, Eq)

instance Ord Packet where
  compare (Value v1) (Value v2) = compare v1 v2
  compare (List l1) (List l2) = compare l1 l2
  compare (Value v1) (List l2) = compare [Value v1] l2
  compare (List l1) (Value v2) = compare l1 [Value v2]

packetsParser :: GenParser Char st [Packet]
packetsParser = packet `Text.Parsec.sepBy` many newline
  where
    packet = list <|> leaf
    leaf = Value <$> digits
    list = List <$> between (char '[') (char ']') (packet `Text.Parsec.sepBy` char ',')
    digits = read <$> many1 digit

main :: IO ()
main = do
  ls <- readFile "input.txt"
  let parsedA = chunksOf 2 $ fromRight [] $ parse packetsParser "" ls
  let parsedB = fromRight [] $ parse packetsParser "" ls
  let dividerpackets = [List [List [Value 2]], List [List [Value 6]]]
   in do
        print $ sum $ map fst $ filter snd $ zipWith (\ a b -> (a, head b < (b !! 1))) [1 ..] parsedA
        print $ product $ map fst $ filter (\(i, p) -> p `elem` dividerpackets) $ zip [1 ..] $ sort (parsedB ++ dividerpackets)
