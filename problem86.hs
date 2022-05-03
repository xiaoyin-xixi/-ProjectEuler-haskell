import Data.List

main :: IO ()
main = do
    print $ problem86 1 0

limit :: Integer
limit = 1000000

problem86 :: Integer -> Integer -> Integer
problem86 m ttl
    | ttl > limit = m - 1
    | otherwise = problem86 (m+1) (ttl+part)
    where
        part = loop 2 0
        loop n cnt
            | n > 2*m = cnt
            | isSquare sq = loop (n+1) (cnt+cnt')
            | otherwise = loop (n+1) cnt
            where
                sq = m^2 + n^2
                cnt' = if n > m + 1 then (2*(m+1)-n)`div`2 else n `div` 2

isSquare :: Integer -> Bool
isSquare n
    | (n`mod`256) `notElem` modList = False
    | otherwise = loop [1..isqrt n]
    where
        loop :: [Integer] -> Bool
        loop [] = False
        loop (m:ms)
            | m*m == n = True
            | otherwise = loop ms

modList :: [Integer]
modList = nub [(x*x)`mod`256 | x <- [0..255]]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
