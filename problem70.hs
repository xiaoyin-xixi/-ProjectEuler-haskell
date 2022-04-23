import Data.List
import Data.Numbers.Primes
import Data.Ratio

main :: IO ()
main = do
    print $ problem70


problem70 :: Integer
problem70 = fst $ foldl (\(n',phi') (n,phi) -> if (n % phi) < (n' % phi') then (n,phi) else (n',phi')) (10000000,1) candidateList
    where
        eulerList = map (\n -> (n,eulerFunction n)) [2..10000000]
        isPermutation (n,phi) = (sort $ show n) == (sort $ show phi)
        candidateList = filter isPermutation eulerList

eulerFunction :: Integer -> Integer
eulerFunction n = foldl (\acc (x,y) -> acc * (x^y - x^(y-1))) 1 $ divisors n

divisors :: Integer -> [(Integer, Int)]
divisors n = map (\x -> (head x, length x)) $ group $ primeFactors n
