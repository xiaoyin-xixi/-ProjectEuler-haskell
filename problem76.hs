
main :: IO ()
main = do
    print $ problem76

problem76 :: Integer
problem76 = countExpressions 100 - 1
    where
        countExpressions n = foldl (\acc k -> acc + loop n k) 0 [1..n]
            where
                loop 1 _ = 1
                loop _ 1 = 1
                loop n k
                    | k > n-k = loop (n-1) (k-1)
                    | otherwise = loop (n-k) k + loop (n-1) (k-1)
