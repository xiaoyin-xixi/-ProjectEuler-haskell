import Data.List

main :: IO ()
main = do
    print $ problem111

problem111 :: Integer
problem111 = sum $ loop 0 []
    where
        loop 10 r = r
        loop d' r = loop (d'+1) $ loop' 9 d' ++ r
        loop' n d
            | null primeList = loop' (n-1) d
            | otherwise = primeList
            where
                primeList = filter isPrime $ concatMap makeNumbers (makeConb (show d) n)

makeConb :: String -> Int -> [[[String]]]
makeConb numText len = map (\cs -> loop cs 0 []) conbList
    where
        loop :: [Int] -> Int -> [[String]] -> [[String]]
        loop _ 10 r = r
        loop cs cnt r
            | cnt `elem` cs = loop cs (cnt+1) (r++[seqList])
            | otherwise = loop cs (cnt+1) (r++[[numText]])
        seqList = map show [0..9]
        l = 10 - len
        list = map show $ if l < 3 then [0..10^l-1] else drop (10^(l-1)-1) [0..10^l-1]
        list' = nub $ map sort $ filter (\n -> length (nub n) == l) $ map ((reverse . take l . reverse) . ("0" ++)) list
        conbList = map (map (\c -> read [c])) list'

makeNumbers :: [[String]] -> [Integer]
makeNumbers gs = map read $ loop (delete "0" $head gs) (tail gs)
    where
        loop = foldl (\acc n -> makeDigit acc n [])

makeDigit :: [String] -> [String] -> [String] -> [String]
makeDigit numTexts gs list = foldr (\g acc -> loop g numTexts ++ acc) list gs
    where
        loop s = map (++ s)

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = all (\m -> n `mod` m /= 0) [3,5..(truncate $ sqrt $ fromIntegral n)]

