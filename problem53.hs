import Data.Array

main :: IO ()
main = do
    print $ length problem53

problem53 = loop [1..100]
    where
        loop [] = []
        loop p@(r:rs) = [(n,r) | n <- p, comb n r > 10^6] ++ loop rs

facArray :: Array Integer Integer
facArray = listArray (1, 100) $ scanl1 (*) [1..100]

comb :: Integer -> Integer -> Integer
comb n r = if n==r then 1 else (facArray ! n) `div` ((facArray ! r)*(facArray ! (n-r) ))
