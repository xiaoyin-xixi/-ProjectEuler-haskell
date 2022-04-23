import Data.List

main :: IO ()
main = do
    print $ snd $ minimumBy (\x y -> compare (snd x) (snd y)) $ concat $ search 5


search :: Int -> [[(String, Integer)]]
search n = filter (\x -> length x==n) $ collect $ sort cubicNumList
    where
        collect [] = []
        collect (x:xs) = collect' xs (fst x) [x]
        collect' (x:xs) cur list
            | fst x == cur = collect' xs cur (x:list)
            | otherwise = collect xs ++ [list]

sortedNum :: Integer -> String
sortedNum n = sort $ show n

cubicNumList :: [(String, Integer)]
cubicNumList = take 10000 [(sortedNum $ x^3, x^3) | x <- [1..]]
