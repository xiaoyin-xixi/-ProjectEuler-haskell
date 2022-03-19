
main :: IO ()
main = do
    print $ sum . filter even $ takeWhile (<4*10^6) fibo

fibo :: [Integer]
fibo = 1:1:zipWith (+) fibo (tail fibo)
