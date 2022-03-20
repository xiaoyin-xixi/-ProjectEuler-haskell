import Data.List

main :: IO ()
main = do
    print $ problem21 10000

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

divisors :: Integer -> [Integer]
divisors n = loop [1..isqrt n]
    where
        loop :: [Integer] -> [Integer]
        loop [] = []
        loop (m:ms) =
            let ds = case n `mod` m of
                        0 -> if m*m == n then [m] else [m, div n m]
                        otherwisw -> []
            in ds ++ loop ms

problem21 :: Integer -> Integer
problem21 n = foldl (\acc (x1, x2) -> acc + x1 + x2) 0 searchPair
    where
        loop [] = []
        loop (m:ms) =
            let ds = divisors m
            in [(m, sum ds)] ++ loop ms

        candidateList = filter (\x -> length x > 1) $
                        groupBy (\(m1, n1) (m2, n2) -> n1 == n2) $
                        sortBy (\(_, m1) (_, m2) -> compare m1 m2) $ loop [1..n]

        combination2 [] = []
        combination2 (x:xs) = comb x xs ++ combination2 xs
            where
                comb _ [] = []
                comb b (n:ns) = [[b, n]] ++ comb b ns

        searchPair = [(y1,y2) | y@((y1,n1):(y2,n2):ys) <- concat [combination2 x | x <- candidateList], y1 + y2 == n1]
