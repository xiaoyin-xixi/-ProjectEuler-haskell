import Data.List.Split
import Data.List

main :: IO ()
main = do
    list <- readSetList
    print $ problem105 list

problem105 :: [[Int]] -> Int
problem105 list = sum $ map sum $ filter (isSpecialSubsetSums . makeSemigroupPair) list


isSpecialSubsetSums :: [([Int],[Int])] -> Bool
isSpecialSubsetSums = all (\(b,c) -> isCondition1 b c && isCondition2 b c)
    where
        isCondition1 b c = sum b /= sum c
        isCondition2 b c = (length b <= length c) || (sum b > sum c)

makeSemigroupPair :: [Int] -> [([Int], [Int])]
makeSemigroupPair xs = [(b,c) | b <- sg, c <- sg, isEmpty (b,c)]
    where
        isEmpty (b,c) = not $ any (`elem` b) c
        sg = filter (\ns -> not (null ns) && length ns < length xs) $ subsequences xs


readSetList :: IO [[Int]]
readSetList = do
    set <- readFile "p105_sets.txt"
    let list = map (splitOn ",") $ lines set
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Int) x : loop xs
