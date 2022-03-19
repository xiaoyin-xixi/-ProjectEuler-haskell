import Data.Numbers.Primes

main :: IO ()
main = do
    print $ sum $ takeWhile (<=2*10^6) primes

