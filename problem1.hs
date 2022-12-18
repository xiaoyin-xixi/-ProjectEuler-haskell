import Data.List

main :: IO ()
main = do
    print $ problem1 1000

problem1 :: Int -> Int 
problem1 n = sum $ union [3,6..n-1] [5,10..n-1]
