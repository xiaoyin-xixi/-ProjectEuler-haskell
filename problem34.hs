
main :: IO ()
main = do
    print $ problem34

problem34 :: Integer
problem34 = sum [x | x <- [3..(10^7)], isSame x]
facList :: [Integer]
facList = [1,1,2,3*2,4*3*2,5*4*3*2,6*5*4*3*2,7*6*5*4*3*2,8*7*6*5*4*3*2,9*8*7*6*5*4*3*2]


isSame :: Integer -> Bool
isSame n = n == foldl (\acc x -> acc + facList !! read [x]) 0 nList
    where
        nList = show n
