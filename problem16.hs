
main :: IO ()
main = do
    print $ foldl (\x y -> x + read [y]) 0 $ show (2^1000)

