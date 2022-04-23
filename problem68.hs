import Data.List

main :: IO ()
main = do
    print $ maximum $ filter (<10000000000000000) $ map makeConcatNum $ search5gon $ permutations [1..10]

makeConcatNum :: [[Int]] -> Integer
makeConcatNum xs = concatNum $ adjust idx numList
    where
        concatNum list = read (foldl (\acc x -> acc ++ show x) "" list) :: Integer
        numList = map concatNum xs
        minValue = minimum numList
        (Just idx) = elemIndex minValue numList
        adjust 0 ys = ys
        adjust cnt (y:ys) = adjust (cnt-1) (ys++[y])

search5gon :: [[Int]] -> [[[Int]]]
search5gon [] = [] 
search5gon (x:xs)
    | check5gon ring = ring : search5gon xs
    | otherwise = search5gon xs
    where
        ring = make5gonRing x
        check5gon xs = all (n==) ns
            where 
                (n:ns) = map sum xs

make5gonRing :: [Int] -> [[Int]]
make5gonRing xs = [x1, x2, x3, x4, x5]
    where
        x1 = [xs !! 5, xs !! 1, xs !! 2] 
        x2 = [xs !! 6, xs !! 2, xs !! 3]
        x3 = [xs !! 7, xs !! 3, xs !! 4]
        x4 = [xs !! 8, xs !! 4, xs !! 0]
        x5 = [xs !! 9, xs !! 0, xs !! 1]
