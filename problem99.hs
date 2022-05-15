import Data.List.Split

main :: IO ()
main = do
    list <- readPairList
    print $ problem99 list

problem99 :: [(Integer, Integer)] -> Integer
problem99 list = loop list 0 1 1
    where
        loop [] _ _ r = r
        loop (p:ps) v cnt r
            | v > a = loop ps v (cnt+1) r
            | otherwise = loop ps a (cnt+1) cnt
            where
                a = uncurry (^) p

readPairList :: IO [(Integer, Integer)]
readPairList = do
    list <- readFile "p099_base_exp.txt"
    let l = map (splitOn ",") $ lines list
    return $ map (\a -> (read (a!!0), read (a!!1))) l
