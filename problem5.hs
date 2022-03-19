
main :: IO ()
main = do
    print $ problem5 20

problem5 :: Integer -> Integer
problem5 n = foldl lcm 1 [1..n]
