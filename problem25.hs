import Data.List

main :: IO ()
main = do
    print $ (+1) <$> findIndex (>10^(1000-1)) fibo

fibo :: [Integer]
fibo = 1:1:zipWith (+) fibo (tail fibo)
