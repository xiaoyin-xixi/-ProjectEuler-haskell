
main :: IO ()
main = do
    print $ foldl (\acc xs -> acc + sumDiag xs) 0 $ makeNumList 1001


sumDiag :: [Integer] -> Integer
sumDiag xs = loop 0 0
    where
        l = length xs
        loop i acc
            | l == 1 = 1
            | i' > l = acc
            |otherwise = loop i' (acc + xs !! (i'-1))
            where
                i' = i + div l 4


makeNumList :: Integer -> [[Integer]]
makeNumList n = loop 2 2 [[1]]
    where
        loop s m xs 
            | m > n = xs
            | otherwise = loop m' (m+2) ([s..(m'-1)]:xs)
            where
                m' = 4*m+s

