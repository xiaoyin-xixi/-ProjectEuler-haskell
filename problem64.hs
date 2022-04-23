import Data.List

main :: IO ()
main = do
    print $ problem64

maxNum :: Integer
maxNum = 10000
problem64 :: Int
problem64 = length $ filter odd $ map sequenceCycleLength $ [x | x <- [2..maxNum]] \\ [y^2 | y <- [2..maxNum], y^2 <= maxNum]

sequenceCycleLength :: Integer -> Integer
sequenceCycleLength n = loop (isqrt n) 0 1 0
    where
        w0 = sqrt $ fromIntegral n :: Double
        a0 = floor w0
        loop a l p cnt
            | a0*2 == a = cnt
            | otherwise = loop a' l' p' (cnt+1)
            where
                a' = floor w'
                l' = p * a - l
                p' = (n - l'^2) `div` p
                w' = (w0 + fromIntegral l') / fromIntegral p'

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
