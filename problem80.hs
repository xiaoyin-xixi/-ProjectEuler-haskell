import Data.Ratio
import Data.List

main :: IO ()
main = do
    print $ problem80
            
problem80 :: Integer
problem80 = foldl (\acc n -> acc + sumDigit (squreRoot n)) 0 list
    where
        list = [2..100] \\ [y^2 | y <- [2..100], y^2 <= 100]

sumDigit :: Integer -> Integer
sumDigit n = foldl (\acc n -> acc + read [n]) 0 $ take 100 $ show n

squreRoot :: Integer -> Integer
squreRoot n = head sc * 10^limit + decimalPart
    where
        limit = 100
        frac = loop (tail cycList) (fromIntegral $ head cycList)
        decimalPart = (denominator frac  * 10^limit) `div` numerator frac
        sc = sequenceCycle n
        cycList = reverse $ tail sc
        loop cyc cur
            | null cyc = loop cycList next
            | isSame cur next = cur
            | otherwise = loop cycList next
            where
                next = fraction cyc cur
                isSame r1 r2 = (denominator r1 * 10^limit) `div` numerator r1 == (denominator r2 * 10^limit) `div` numerator r2 

fraction :: [Integer] -> Rational -> Rational
fraction xs s = loop xs s
    where
        loop [] r = r
        loop (x:xs) r = loop xs d
            where
                d = fromIntegral x + 1/r

sequenceCycle :: Integer -> [Integer]
sequenceCycle n = loop (isqrt n) 0 1 []
    where
        w0 = sqrt $ fromIntegral n :: Double
        a0 = floor w0
        loop a l p sc
            | a0*2 == a = reverse (a:sc)
            | otherwise = loop a' l' p' (a:sc)
            where
                a' = floor w'
                l' = p * a - l
                p' = (n - l'^2) `div` p
                w' = (w0 + fromIntegral l') / fromIntegral p'

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
