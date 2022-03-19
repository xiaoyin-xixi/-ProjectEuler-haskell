import Data.Numbers.Primes

main :: IO ()
main = do
    print $ maximum $ primeFactors 600851475143

