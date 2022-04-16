import Data.Numbers.Primes (primes)
import Data.List

main :: IO ()
main = do
    let (Just x) = problem60 list list
    print $ sum x
        where
            list = pairPrimesList filteredPrimeList

problem60 :: [(Integer, [Integer])] -> [(Integer, [Integer])] -> Maybe [Integer]
problem60 [] _ = Nothing
problem60 (x:xs) list
    | search == Nothing = problem60 xs list
    | otherwise = search
    where
        search = matchPair [fst x] 4 (snd x) pairList
        pairList = reverse $ candidatePairList x list
        matchPair res 0 [x] _ = Just res
        matchPair _ _ _ [] = Nothing
        matchPair _ _ [] _ = Nothing
        matchPair res n pl (x:xs)
            | n > len = Nothing
            | m' == Nothing = matchPair res n pl xs
            | n == length inter = m'
            | otherwise = matchPair res n inter xs
            where
                len = length pl
                inter = intersect pl (fst x:snd x)
                m' = matchPair (fst x:res) (n-1) inter xs

candidatePairList :: (Integer, [Integer]) -> [(Integer, [Integer])] -> [(Integer, [Integer])]
candidatePairList x list = loop $ snd x
    where
        loop [] = []
        loop (x:xs) = loop xs ++ [get x list]
            where
                get p (x:xs)
                    | p == fst x = x
                    | otherwise = get p xs

pairPrimesList :: [Integer] -> [(Integer, [Integer])]
pairPrimesList [] = []
pairPrimesList (p:ps) = pairPrimesList ps ++ [loop p [] $ takeWhile (<p) filteredPrimeList]
    where
        loop p l [] = (p,l)
        loop p l (x:xs)
            | checkPairPrimes x p = loop p (x:l) xs
            | otherwise = loop p l xs


checkPairPrimes :: Integer -> Integer -> Bool
checkPairPrimes n m = isPrime p1 && isPrime p2
    where
        p1 = read (show n ++ show m) :: Integer
        p2 = read (show m ++ show n) :: Integer

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = all (\m -> n `mod` m /= 0) [3,5..(truncate $ sqrt $ fromIntegral n)]

filteredPrimeList :: [Integer]
filteredPrimeList = takeWhile (<10000) primes