import Data.Numbers.Primes
import Data.List ( group )

main :: IO ()
main = do
    print $ fst problem12

triNums :: [Integer]
triNums = scanl (+) 1 [2..]

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..n], n `mod` x == 0]

divisors_num :: Integer -> Int
divisors_num n = foldl (\x y -> x * (length y + 1)) 1 $ group $ primeFactors n

problem12 :: (Integer, Int)
problem12 = head [z | z <-[(x, divisors_num x) | x <- triNums], snd z >= 500]
