
main :: IO ()
main = do
    print $ countCoin 0 [200, 100, 50, 20, 10, 5, 2, 1]

countCoin :: Integer -> [Integer] -> Integer
countCoin 200 _ = 1
countCoin _ [] = 0
countCoin sum l@(x:xs)
    | sum > 200 = 0
    | otherwise = a + b
    where
        a = countCoin sum xs
        b = countCoin (sum + x) l

