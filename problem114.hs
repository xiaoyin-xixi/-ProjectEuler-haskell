
main :: IO ()
main = do
    print $ problem114 3 50

problem114 :: Integer -> Integer -> Integer
problem114 m n = loop [2..n] []
    where
        loop [] list = last list
        loop (x:xs) list = loop xs (list ++ [cnt])
            where
                cnt = f m x list
        

f :: Integer -> Integer -> [Integer] -> Integer
f m n list = count m n
    where
        count minBlock space
            | space == m = 2
            | otherwise = s' + 1 + foldl (\acc s -> acc - 1 + list!!fromIntegral(s - 2)) acc [minBlock..s'-1]
                where
                    s' = space - minBlock
                    acc = count minBlock (space - 1)
