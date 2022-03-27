
main :: IO ()
main = do
    print $ sum problem30

problem30 = [x | x <- [2..(9^5*6)], expSum x == x]

expSum :: Integer -> Integer
expSum n = foldl (\acc c -> acc + (read [c])^5) 0 $ show n


