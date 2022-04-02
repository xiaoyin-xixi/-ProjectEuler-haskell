import Data.List

main :: IO ()
main = do
    print $ sum $ map read problem43

problem43 :: [[Char]]
problem43 = filter isValid pandigitalList
    where
        isValid t = (d8_10 `mod` 17 == 0) && (d7_9 `mod` 13 == 0) && (d6_8 `mod` 11 == 0) &&
                    (d5_7 `mod` 7 == 0) && (d4_6 `mod` 5 == 0) && (d3_5 `mod` 3 == 0) && (d2_4 `mod` 2 == 0)
            where
                d2_4 = read (t !! 1 : t !! 2 : t !! 3 : []) :: Integer
                d3_5 = read (t !! 2 : t !! 3 : t !! 4 : []) :: Integer
                d4_6 = read (t !! 3 : t !! 4 : t !! 5 : []) :: Integer
                d5_7 = read (t !! 4 : t !! 5 : t !! 6 : []) :: Integer
                d6_8 = read (t !! 5 : t !! 6 : t !! 7 : []) :: Integer
                d7_9 = read (t !! 6 : t !! 7 : t !! 8 : []) :: Integer
                d8_10 = read (t !! 7 : t !! 8 : t !! 9 : []) :: Integer


pandigitalList :: [[Char]]
pandigitalList = permutations "1234567890"
