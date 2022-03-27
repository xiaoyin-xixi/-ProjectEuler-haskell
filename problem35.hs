import Data.Numbers.Primes

main :: IO ()
main = do
    print $ problem35

problem35 = length [x | x <- filteredPrimeList, isValid x]
    where
        isValid n = all (flip elem filteredPrimeList) $ makeCircularList n

makeCircularList :: Integer -> [Integer]
makeCircularList n = loop 1 n'
    where
        n' = show n
        l = length n'
        loop m (x:xs)
            | m > l = []
            | otherwise = [read c]++ loop (m+1) c
                where
                    c = (xs++[x])

primeList :: [Integer]
primeList = takeWhile (<10^6) primes

filteredPrimeList :: [Integer]
filteredPrimeList = [2] ++ filter (\n -> and $ map (\s -> s == '1' || s == '3' || s == '5' || s == '7' || s == '9') (show n)) primeList

