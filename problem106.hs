import Data.List

main :: IO ()
main = do
    print $ length $ problem106 $ filteredSemigroupPair [1..12]

problem106 :: [([Int], [Int])] -> [([Int], [Int])]
problem106 xs = loop xs []
    where
        loop [] list = list
        loop (x:xs) list
            | loop' b c flag = loop xs (x:list)
            | otherwise = loop xs list
            where
                flag = head (fst x) > head (snd x)
                b = tail $ fst x
                c = tail $ snd x
        loop' [] [] _ = False
        loop' [] _ _ = False
        loop' _ [] _ = False
        loop' (x:xs) (y:ys) flag
            | flag && x > y = loop' xs ys flag
            | not flag && x < y = loop' xs ys flag
            | otherwise = True

filteredSemigroupPair :: [Int] -> [([Int], [Int])]
filteredSemigroupPair xs = loop sg []
    where
        loop [] list = list
        loop (x:xs) list = loop xs (loop' (x:xs) xs list)
        loop' _ [] list = list
        loop' xs (y:ys) list
            | isSameLen p && isEmpty p = loop' xs ys (p:list)
            | otherwise = loop' xs ys list
            where
                p = (head xs,y)
                isSameLen (b,c) = length b > 1 && length c > 1 && length b == length c
                isEmpty (b,c) = not $ any (`elem` b) c
        sg = filter (\ns -> not (null ns) && length ns < length xs) $ subsequences xs
