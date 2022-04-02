import Data.List
import Data.Numbers.Primes

main :: IO ()
main = do
    print $ maximum $ filter isPandigital primeList

isPandigital :: Integer -> Bool
isPandigital n = null $ t \\ (concat $ map show [1..len])
    where
        t = show n
        len = length t

primeList :: [Integer]
primeList = takeWhile (<10^9) primes
