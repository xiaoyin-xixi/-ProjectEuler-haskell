
main :: IO ()
main = do
    print $ maximum [digitSum (a^b) | a <- [1..99], b <- [1..99]]

digitSum :: Integer -> Integer
digitSum n = foldl (\acc x -> acc + read [x]) 0 $ show n

