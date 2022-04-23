import Data.Ratio
import Data.List

main :: IO ()
main = do
    print $ problem73

problem73 :: Int
problem73 = length $ filter (\x -> 1%3 < x && x < 1%2) list
    where 
        list = sort [n%d | d <- [2..12000], n <- [1..d-1], gcd n d == 1]
