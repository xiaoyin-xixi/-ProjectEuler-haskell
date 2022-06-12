
main :: IO ()
main = do
    print $ problem116 4 50 + problem116 3 50 + problem116 2 50

problem116 :: Integer -> Integer -> Integer
problem116 m n = loop [2..n] []
    where
        loop [] list = last list
        loop (x:xs) list = loop xs (list ++ [cnt])
            where
                cnt = f m x list

f :: Integer -> Integer -> [Integer] -> Integer
f m n list = count m n
    where
        count minBlock space
            | space == m = 1
            | otherwise = s' + 1 + foldl (\acc s -> acc + list!!fromIntegral(s - 2)) 0 [minBlock..s']
                where
                    s' = space - minBlock
