
main :: IO ()
main = do
    print $ problem55

problem55 :: Int
problem55 = length $ filter (==False) $ map (\n -> testPalindromic 50 n) [10..10000-1]

isPalindromic :: Integer -> Bool 
isPalindromic n = t1 == t2
    where
        t1 = show n
        t2 = reverse t1

reverseNum :: Integer -> Integer
reverseNum n = read $ reverse $ show n

testPalindromic :: Int -> Integer -> Bool
testPalindromic 0 _ = False
testPalindromic cnt n
    | isPalindromic (t+n) = True
    | otherwise = testPalindromic (cnt-1) (t+n)
    where
        t = reverseNum n

