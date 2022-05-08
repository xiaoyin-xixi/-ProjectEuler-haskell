import Data.List

main :: IO ()
main = do
    print $ problem94

limit :: Integer
limit = 1000000000 `div` 3

problem94 :: Integer
problem94 = foldl (\acc n -> acc + getLength n) 0 [3..limit]

getLength :: Integer -> Integer
getLength n = loop list 0
    where
        list = [(n,n,n-1),(n,n,n+1)]
        loop [] len = len
        loop ((a,b,c):xs) len
            | isInteger s = loop xs (len+l)
            | otherwise = loop xs len
            where
                l = a + b + c
                s = l*(l-2*a)*(l-2*b)*(l-2*c)

isInteger :: Integer -> Bool
isInteger n
    | n `mod` 16 == 0 = isSquare (n`div`16)
    | otherwise = False
    where
        isSquare n = (floor . sqrt . fromIntegral) n ^2 == n
