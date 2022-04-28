import Data.List
import Data.Numbers.Primes

main :: IO ()
main = do
    print $ problem77

problem77 :: Integer
problem77 = loop [1..]
    where
        loop (x:xs)
            | cnt >= 5000 = x
            | otherwise = loop xs
            where
                cnt = countNum [] x

countNum :: [Integer] -> Integer -> Integer
countNum l n = loop 0 $ take 100 primes
    where
        loop cnt [] = cnt
        loop cnt (p:ps)
            | not (null l) && head l > p = loop cnt ps
            | rem > 0 = loop (cnt + countNum (p:l) rem) ps
            | rem == 0 = loop (cnt+1) ps
            | otherwise = loop cnt ps
                where
                    rem = n - p
