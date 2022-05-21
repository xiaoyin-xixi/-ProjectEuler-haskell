-- n=6 A={11,18,19,20,22,25}
-- n=7 predict A={20,32,38,39,40,42,45}

import Data.List

main :: IO ()
main = do
    print $ problem103

problem103 :: String
problem103 = foldl (\acc n -> acc ++ show n) [] optimumSet
    where
        optimumSet = fst $ minimumBy (\a b -> compare (snd a) (snd b)) pairList
        pairList = map (\a -> (a, sum a)) $ filter (isSpecialSubsetSums . makeSemigroupPair) candidateSpecialSubsetSums

candidateSpecialSubsetSums :: [[Int]]
candidateSpecialSubsetSums = nub $ filter (\a -> length (nub a) == 7) $ [sort [a1,a2,a3,a4,a5,a6,a7] | 
                            a1 <- [19..21], a2 <- [31..33], a3 <- [37..39], a4 <- [38..40], a5 <- [39..41], a6 <- [41..43], a7 <- [44..46]]
        
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
