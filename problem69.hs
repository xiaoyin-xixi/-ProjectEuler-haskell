import Data.List
import Data.Numbers.Primes
import Data.Ratio

main :: IO ()
main = do
    print $ problem69


problem69 :: Integer
problem69 = fst $ foldl (\(n',phi') (n,phi) -> if (n % phi) > (n' % phi') then (n,phi) else (n',phi')) (1,1) eulerList
    where
        eulerList = map (\n -> (n,eulerFunction n)) [2..1000000]

eulerFunction :: Integer -> Integer
eulerFunction n = foldl (\acc (x,y) -> acc * (x^y - x^(y-1))) 1 $ divisors n

divisors :: Integer -> [(Integer, Int)]
divisors n = map (\x -> (head x, length x)) $ group $ primeFactors n

