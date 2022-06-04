-- method of Lagrange multiplier
-- f = Sig(ek * log pk)k=1->n
-- g = Sig(log(2ek + 1))k=1->n - log M
-- M = limit'
-- h = f - lambda * g
-- dh/dek = log pk - 2lambda/(2ek + 1) = 0
-- 2ek + 1 = 2lambda/log pk
-- Pi(2ek + 1)k=1->n = M
-- (2lambda)^n/Pi(log pk)k=1->n = M
-- lambda = (M*Pi(log pk)k=1->n)^(1/n)/2
-- ek = lambda/log pk - 1/2

import Data.Numbers.Primes
import Data.List

main :: IO ()
main = do
    print $ problem110

limit :: Integer
limit = 4*10^6
limit' :: Integer
limit' = 2*limit + 1

problem110 :: Integer
problem110 = calcN $ fst $ head list
    where
        list = sortBy (\(_,n1) (_,n2) -> compare n1 n2) $ map (\n -> getMin n limit' 1) [1..numPrimes]

getMin :: Integer -> Integer -> Integer -> ([Integer], Double)
getMin 1 m _ = ([m`div`2], fromIntegral (m`div`2)*logPrimes!!0)
getMin n m eMin
    | eMax == 1 = f 1
    | otherwise = mines [e-1,e-2..eMin] $ mines [e..eMax] $ f e
    where
        e = calcE n m
        eMax = truncate ((fromIntegral m**(1/fromIntegral n) - 1)/2)
        f e = (es++[e], fromIntegral e*logPrimes!!fromIntegral(n-1) + v)
            where
                (es, v) = getMin (n-1) (divCeil m (2*e + 1)) e
        mines es minPair = loop es minPair minVal'
            where
                minVal' = calcN $ fst minPair
                loop [] minp _ = minp
                loop (e:es) minp minVal
                    | v > snd minp = minp
                    | otherwise = if val < minVal then loop es pair val else loop es minp minVal
                    where
                        pair = f e
                        val = calcN $ fst pair 
                        m' = divCeil m (2*e + 1)
                        v = fromIntegral e*logPrimes!!fromIntegral(n-1) + calcF (n-1) m'

numPrimes :: Integer
numPrimes = truncate (log (fromIntegral limit') / log 3 + 1)

primeList :: [Integer]
primeList = take (fromIntegral numPrimes) primes

logPrimes :: [Double]
logPrimes = map (log . fromIntegral) primeList

calcF :: Integer -> Integer -> Double
calcF n m = sum $ zipWith (*) lps es
    where
        lps = take (fromIntegral n) logPrimes
        lambda = ((fromIntegral m * product lps) ** (1/fromIntegral n)) / 2
        es = map (\x -> lambda/x - 0.5) lps

calcE :: Integer -> Integer -> Integer
calcE n m = truncate (lambda / last lps - 0.5)
    where
        lps = take (fromIntegral n) logPrimes
        lambda = ((fromIntegral m * product lps) ** (1/fromIntegral n)) / 2

calcN :: [Integer] -> Integer
calcN es = product $ zipWith (^) primeList es

divCeil :: Integer -> Integer -> Integer
divCeil n m = (n + m - 1)`div`m