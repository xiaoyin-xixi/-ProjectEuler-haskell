import Data.Numbers.Primes
import Data.List

main :: IO ()
main = do
    print $ problem47

problem47 :: Integer
problem47 = loop 2 0 [length $ group $ primeFactors n | n <- [2..]]
    where
        loop n 4 _ = n
        loop _ _ [] = 0
        loop n c (x:xs)
            | x == 4 = loop n (c+1) xs
            | otherwise = loop (n+1+c) 0 xs

