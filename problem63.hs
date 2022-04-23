
main :: IO ()
main = do
    print $ problem63 0 [1..]

problem63 :: Int -> [Integer] -> Int
problem63 acc (x:xs)
    | c == 0 = acc
    | otherwise = problem63 (acc + c) xs
    where
        c = countNDigitNum x

countNDigitNum :: Integer -> Int
countNDigitNum n = length $ dropWhile (\x -> (fromIntegral $ length $ show x) < n) $ takeWhile (\x -> (fromIntegral $ length $ show x) <= n) [x^n | x <- [1..]]

