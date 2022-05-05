
main :: IO ()
main = do
    print $ problem92

limit :: Int
limit = 10000000

problem92 :: Int
problem92 = length $ filter (==89) $ map loop [1..limit-1]
    where
        loop 1 = 1
        loop 89 = 89
        loop n = loop $ sumSquare n 

sumSquare :: Int -> Int
sumSquare n = foldl (\acc c -> acc + read [c]^2) 0 $ show n
