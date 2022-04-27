import Data.Array

-- a = m^2 - n^2
-- b = 2mn
-- c = m^2 + n^2
-- a+b+c = 2m(m+n)

main :: IO ()
main = do
    print $ problem75

problem75 :: Int
problem75 = length $ filter (==1) $ elems $ foldl (\acc l -> loop l acc [] [1..]) countArray list
    where
        list = filter (<1500000) $ map (\(m,n) -> 2*m*(m+n)) [(m,n) | m <- [2..1000], n <- [1..m-1], (m-n)`mod`2 == 1, gcd m n == 1]
        countArray = listArray (1,1500000) $ take 1500000 $ repeat 0
        loop l arr t (x:xs)
            | n > 1500000 = arr // reverse t
            | otherwise = loop l arr (n `seq` s `seq` (n,s):t) xs
            where
                n = l*x
                s = arr!n + 1 
