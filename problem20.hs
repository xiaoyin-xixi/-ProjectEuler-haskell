
main :: IO ()
main = do
    print $ foldl (\x y -> x + read [y]) 0 $ show $ foldl (*) 1 [1..100]

