
main :: IO ()
main = do
    print $ primes !! (10001-1)

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

primes :: [Integer]
primes = sieve [2..]
