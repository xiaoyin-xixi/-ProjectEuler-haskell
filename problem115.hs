
main :: IO ()
main = do
    print $ problem115

problem115 :: Integer
problem115 = loop 50
    where
        loop cnt
            | num > 1000000 = cnt
            | otherwise = loop (cnt+1)
            where
                num = f 50 cnt

f :: Integer -> Integer -> Integer
f m n = count m n
    where
        count minBlock space
            | space == m = 2
            | otherwise = s' + 1 + foldl (\acc s -> acc - 1 + count minBlock s) acc [minBlock..s'-1]
                where
                    s' = space - minBlock
                    acc = count minBlock (space - 1)
