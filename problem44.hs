
main :: IO ()
main = do
    print $ problem44

problem44 :: Integer
problem44 = head $ loop scanList
    where
        loop [] = []
        loop (x:xs) = [n-x | n <- xs, isValid x n] ++ loop xs
        isValid x y = (elem (x+y) pentagonList) && (elem (y-x) pentagonList)

len :: Integer
len = 5000

pentagonList :: [Integer]
pentagonList = map (\n -> n*(3*n-1) `div` 2) [1..len]

scanList :: [Integer]
scanList = take (fromIntegral len `div` 2) pentagonList
