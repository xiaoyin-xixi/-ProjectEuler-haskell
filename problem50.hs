import Data.Numbers.Primes

main :: IO ()
main = do
    print $ problem50 primeList

problem50 :: [Integer] -> Maybe (Integer, Integer)
problem50 xs = foldl1 max' $ loop xs
    where
        loop [] = []
        loop xs = [searchSum xs] ++ (loop $ tail xs)
        max' a Nothing = a
        max' (Just acc) (Just x) = if fst acc < fst x then Just x else Just acc

primeList :: [Integer]
primeList = takeWhile (<10^6) primes

searchSum :: [Integer] -> Maybe (Integer, Integer)
searchSum (x:xs) = let l = loop x 1 xs in if null l then Nothing else Just (last l)
    where
        loop _ _ [] = []
        loop acc total (x:xs)
            | acc > 10^6 = []
            | otherwise = 
                let n = acc + x
                in  if elem n primeList then [(total+1,n)] ++ loop n (total+1) xs else loop n (total+1) xs

