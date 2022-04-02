import Data.Numbers.Primes
import Data.List
import Distribution.Simple.Utils (xargs)

main :: IO ()
main = do
    print $ problem49 fourDigitPrimeList

problem49 :: [Integer] -> Integer
problem49 xs = con $ head $ dropWhile (==(1487,4817,8147)) $ nub $ loop xs
    where
        loop [] = []
        loop (x:xs) = (searchSequence . getPrimeNum . makePermutationNumList) x ++ loop xs
        con (a,b,c) = read (show a ++ show b ++ show c) :: Integer

fourDigitPrimeList :: [Integer]
fourDigitPrimeList = dropWhile (<1000) $ takeWhile (<10000) primes

makePermutationNumList :: Integer -> [Integer]
makePermutationNumList n = sort $ nub $ map (read) $ permutations $ show n

getPrimeNum :: [Integer] -> [Integer]
getPrimeNum xs = foldl (\acc n -> acc ++ if elem n fourDigitPrimeList then [n] else []) [] xs

searchSequence :: [Integer] -> [(Integer, Integer, Integer)]
searchSequence xs = loop xs
    where
        loop [] = []
        loop [xs] = []
        loop (x:xs) = [(x, n, x+(n-x)*2) | n <- xs, elem (x+(n-x)*2) $ tail xs ] ++ loop xs
