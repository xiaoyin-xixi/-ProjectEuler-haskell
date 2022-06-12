
main :: IO ()
main = do
    print $ problem117 50

problem117 :: Integer -> Integer
problem117 n = loop [2..n] []
    where
        loop [] list = last list
        loop (x:xs) list = loop xs (list ++ [cnt])
            where
                cnt = f x list

f :: Integer -> [Integer] -> Integer
f n list = count n 3
    where
        count space cnt
            | cnt == 0 = 1
            | space == 2 = 2
            | otherwise = s' + 1 + foldl (\acc s -> acc - 1 + list!!fromIntegral(s - 2)) acc [2..s']
                where
                    s' = space - 2
                    acc = count (space - 1) (cnt-1)
