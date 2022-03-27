
main :: IO ()
main = do
    let l = problem27
    print $ l
    print $ mul $ foldl (\p@(a,b,n) p'@(a',b',n') -> if n < n' then p' else p) (0,0,0) l
    where
        mul (a, b, _) = a * b

problem27 :: [(Integer, Integer, Integer)]
problem27 = [(a, b, l) |  a <- [-999..999],b  <- takeWhile (<=1000) primes, let l = lengthPrimes a b 0, l>0]
    where
        lengthPrimes a b n
            | y < 1 = 0
            | isPrime y = lengthPrimes a b (n+1)
            | otherwise = n
            where
                 y = n^2 + a * n + b


sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

primes :: [Integer]
primes = sieve [2..]

expMod :: Integer -> Integer -> Integer
expMod a n = loop a (n-1) n
    where
        loop a n' n
            | n' == 1 = a
            | even n' = (loop a (n' `div` 2) n)^2 `mod` n
            | otherwise = a * (loop a ((n'-1) `div` 2) n )^2 `mod` n

isPrime :: Integer -> Bool
isPrime n = and $ map (\a -> fermatTest n a) [2..(n-1)]
    where
        fermatTest n a
            | gcd n a == 1 = (expMod a n == 1)
            | otherwise = False 
        