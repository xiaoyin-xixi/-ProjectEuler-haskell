import Data.Array
import Data.List

main :: IO ()
main = do
    print $ foldl (\acc xs -> acc + snd xs) 0 <$> problem61

problem61 :: Maybe [(Int, Int)]
problem61 = find (\cs -> macth2Digit (snd $ last cs) (snd $ head cs)) $ filter (\cs -> length cs == 6) $ loop triangleList
    where
        loop [] = []
        loop (x:xs) = search [] x ++ loop xs

search :: [(Int, Int)] -> (Int, Int) -> [[(Int, Int)]]
search t p@(n,m)
    | cs == [] = [[p]]
    | otherwise = map (\c -> p:c) $ concat $ search' cs
    where
        search' xs = map (\x -> search (p:t) x) xs
        cs = candidateList (p:t) $ matchingArrayList ! (lower2Digit $ m)
        matchingArrayList = matchingArray 0 []
        candidateList _ [] = []
        candidateList [] list = list
        candidateList (x:xs) list = candidateList xs (filter (\(n,_) -> fst x /= n) list)

matchingArray :: Int -> [[(Int, Int)]] -> Array Int [(Int, Int)]
matchingArray cnt arr
    | cnt == 100 = listArray(0,99) arr
    | otherwise = matchingArray (cnt+1) (arr ++ [collect cnt list])
    where
        collect _ [] = []
        collect n (x:xs) = loop n x ++ collect n xs 
            where
                loop _ [] = []
                loop n (x:xs)
                    | (higher2Digit $ snd x) == n = x : loop n xs
                    | otherwise = loop n xs
        list = [
            squareList,
            pentagonalList,
            hexagonalList,
            heptagonalList,
            octagonalList
            ]

lower2Digit :: Int -> Int
lower2Digit n = n `mod` 100

higher2Digit :: Int -> Int
higher2Digit n = n `div` 100

macth2Digit :: Int -> Int -> Bool
macth2Digit n m = lower2Digit n == higher2Digit m

triangleList :: [(Int, Int)]
triangleList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (3,n*(n+1)`div`2)) [1..]

squareList :: [(Int, Int)]
squareList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (4,n*n)) [1..]

pentagonalList :: [(Int, Int)]
pentagonalList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (5,n*(3*n-1)`div`2)) [1..]

hexagonalList :: [(Int, Int)]
hexagonalList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (6,n*(2*n-1))) [1..]

heptagonalList :: [(Int, Int)]
heptagonalList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (7,n*(5*n-3)`div`2)) [1..]

octagonalList :: [(Int, Int)]
octagonalList = dropWhile (\p -> snd p < 1000) $ takeWhile (\p -> snd p < 10000) $ map (\n -> (8,n*(3*n-2))) [1..]

