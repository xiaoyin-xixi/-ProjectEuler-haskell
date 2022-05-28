-- 1/x + 1/y = 1/n
-- y = nx / (x-n)


main :: IO ()
main = do
    print $ problem108

problem108 :: Int
problem108 = loop [1000..]
    where
        loop [] = 0
        loop (x:xs)
            | num > 1000 = x
            | otherwise = loop xs
            where
                num = getSolutionNum x

getSolutionNum :: Int -> Int
getSolutionNum n = loop (n+1) 0
    where
        loop x cnt
            | x == 2*n + 1 = cnt
            | remain == 0 = loop (x+1) (cnt+1)
            | otherwise = loop (x+1) cnt
            where
                remain = (n*x) `mod` (x-n)
