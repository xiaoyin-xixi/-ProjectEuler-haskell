-- n = 1000000000000
-- m/n * (m-1)/(n-1) = 1/2
-- 2m(m-1) = n(n-1)
-- (2n-1)^2 -2(2m-1)^2 = -1
-- x^2 - 2y^2 = -1
-- odd x && odd y

import Data.List

main :: IO ()
main = do
    print $ problem100

problem100 :: Integer
problem100 = loop 1 1
    where
        loop x y
            | isOdd = if n' > limit then m' else loop x' y'
            | otherwise = loop x' y'
            where
                x' = x*3 + y*4
                y' = x*2 + y*3
                isOdd = odd x' && odd y'
                n' = (x' + 1) `div` 2
                m' = (y' + 1) `div` 2

limit :: Integer
limit = 1000000000000
