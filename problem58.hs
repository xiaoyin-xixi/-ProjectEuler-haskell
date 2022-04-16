
main :: IO ()
main = do
    print $ problem58

problem58 :: Integer
problem58 = loop 10 4 3 5
    where
        loop s m p a
            | p*100 `div` a < 10 = m-1
            | otherwise = loop m' (m+2) (p+t1+t2+t3+t4) (a+4)
            where
                m' = 4*m+s
                t1 = if isPrime $ s+m-1 then 1 else 0
                t2 = if isPrime $ s+2*m-1 then 1 else 0
                t3 = if isPrime $ s+3*m-1 then 1 else 0
                t4 = if isPrime $ s+4*m-1 then 1 else 0

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = all (\m -> n `mod` m /= 0) [3,5..(truncate $ sqrt $ fromIntegral n)]
