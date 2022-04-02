import Data.Numbers.Primes

main :: IO ()
main = do
    print $ problem46

problem46 :: Integer
problem46 = head $ filter (not . isValid) oddCompsiteList
    where
        f x y = x + 2 * y^2
        isValid n = elem n [f x y | x <- takeWhile (<n) primes, y <- [1..(isqrt n)]]

oddCompsiteList :: [Integer]
oddCompsiteList = filter (not . isPrime') [3,5..]

expMod :: Integer -> Integer -> Integer
expMod a n = loop a (n-1) n
    where
        loop a n' n
            | n' == 1 = a
            | even n' = (loop a (n' `div` 2) n)^2 `mod` n
            | otherwise = a * (loop a ((n'-1) `div` 2) n )^2 `mod` n

isPrime' :: Integer -> Bool
isPrime' n = and $ map (\a -> fermatTest n a) [2..(n-1)]
    where
        fermatTest n a
            | gcd n a == 1 = (expMod a n == 1)
            | otherwise = False 

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
