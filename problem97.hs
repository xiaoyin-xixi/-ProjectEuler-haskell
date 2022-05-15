
main :: IO ()
main = do
    print $ problem97

problem97 :: Integer
problem97 = (28433*2^7830457+1) `mod` 10000000000
