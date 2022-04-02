import Data.Numbers.Primes
import Data.List


main :: IO ()
main = do
    print $ sum problem37

problem37 :: [Integer]
problem37 = [x | x <- droppedPrimesList, isValid $ truncNumList x]
    where
        droppedPrimesList = dropWhile (<10) primesList
        isValid xs = all (flip elem primesList) xs

truncNumList :: Integer -> [Integer]
truncNumList n = 
    let list = nub $ filter (not . null) $ (inits $ show n) ++ (tails $ show n)
    in map read list

primesList :: [Integer]
primesList = takeWhile (<10^6) primes
