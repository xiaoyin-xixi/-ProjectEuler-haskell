
main :: IO ()
main = do
    print $ flip mod 10000000000 $ foldl1 (\acc x -> acc + x^x) [1..1000]

