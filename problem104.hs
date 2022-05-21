import Data.List

main :: IO ()
main = do
    print $ problem104

problem104 :: Int
problem104 = loop 100
    where
        loop cnt
            | isSequence h && isSequence l = cnt + 1
            | otherwise = loop (cnt+1)
            where
                fiboStr = show $ fibo!!cnt
                h = take 9 fiboStr
                l = drop (length fiboStr - 9) fiboStr

isSequence :: String -> Bool
isSequence t = notElem '0' t && length (nub t) == 9 

fibo :: [Integer]
fibo = 1:1:zipWith (+) fibo (tail fibo)
