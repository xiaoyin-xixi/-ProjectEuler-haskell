

main :: IO ()
main = do
    list <- readRomanList
    print $ problem89 list

problem89 :: [String] -> Int
problem89 list = before - after
    where
        before = sum $ map length list
        list' = map (toRoman . toNum) list
        after = sum $ map length list'

toRoman :: Int -> String
toRoman n = loop text d
    where
        loop [] _ = []
        loop (x:xs) digit = (mapList!!(digit-1))!!read [x] ++ loop xs (digit-1)
        text = show n
        d = length text
        mapList = [
            ["","I","II","III","IV","V","VI","VII","VIII","IX"],
            ["","X","XX","XXX","XL","L","LX","LXX","LXXX","XC"],
            ["","C","CC","CCC","CD","D","DC","DCC","DCCC","CM"],
            ["","M","MM","MMM","MMMM"]
            ]

toNum :: [Char] -> Int
toNum text = loop text 0
    where
        loop [] _ = 0
        loop (x:xs) cnt
            | null xs = addRoman x cnt
            | x == 'I' && (x2 == 'V' || x2 == 'X') = loop xs (cnt-1)
            | x == 'X' && (x2 == 'L' || x2 == 'C') = loop xs (cnt-10)
            | x == 'C' && (x2 == 'D' || x2 == 'M') = loop xs (cnt-100)
            | otherwise = loop xs (addRoman x cnt)
            where
                x2 = head xs
                addRoman x cnt
                    | x == 'I' = cnt+1
                    | x == 'V' = cnt+5
                    | x == 'X' = cnt+10
                    | x == 'L' = cnt+50
                    | x == 'C' = cnt+100
                    | x == 'D' = cnt+500
                    | otherwise = cnt+1000

readRomanList :: IO [String]
readRomanList = do
    list <- readFile "p089_roman.txt"
    return $ lines list
