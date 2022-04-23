import Data.Ratio
import Data.List

main :: IO ()
main = do
    print $ fst' $ foldl1 (\(n',x',y') (n,x,y) -> if x' < x then (n,x,y) else (n',x',y')) $ map problem66 ([x | x <- [2..maxNum]] \\ [y^2 | y <- [2..maxNum], y^2 <= maxNum])
        where
            fst' (a,b,c) = a
            
maxNum :: Integer
maxNum = 1000

problem66 :: Integer -> (Integer, Integer, Integer)
problem66 n
    | even n' = (n, x, y)
    | otherwise = (n, x2, y2)
    where
        scl = sequenceCycleLength n
        frac = fraction $ tail scl
        n' = head scl
        x = numerator frac
        y = denominator frac
        x2 = x^2 + n * y^2
        y2 = 2 * x * y


fraction :: [Integer] -> Rational
fraction (x:xs) = loop xs $ fromIntegral x
    where
        loop [] r = r
        loop (x:xs) r = loop xs d
            where
                d = fromIntegral x + (fromIntegral 1)/r 

sequenceCycleLength :: Integer -> [Integer]
sequenceCycleLength n = loop (isqrt n) 0 1 0
    where
        w0 = sqrt $ fromIntegral n :: Double
        a0 = floor w0
        loop a l p cnt
            | a0*2 == a = [cnt]
            | otherwise = (loop a' l' p' (cnt+1)) ++ [a]
            where
                a' = floor w'
                l' = p * a - l
                p' = (n - l'^2) `div` p
                w' = (w0 + fromIntegral l') / fromIntegral p'

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
