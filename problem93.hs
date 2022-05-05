import Data.Ratio
import Data.List
import Data.Maybe

main :: IO ()
main = do
    print $ problem93

problem93 :: String
problem93 = concat $ sort (snd $ maximum resultList)
    where
        resultList = map (\l -> (searchSeqNum 1 $ makeResultList l,l)) combList
        combList = comb 4 ["1","2","3","4","5","6","7","8","9"]
        searchSeqNum _ [] = 0
        searchSeqNum cnt (x:xs)
            | x /= cnt = cnt - 1
            | otherwise = searchSeqNum (cnt+1) xs

makeResultList :: [String] -> [Integer]
makeResultList xs = nub $ sort eList
    where
        permList = permutations xs
        eList = concatMap positiveList permList
        positiveList xs = map numerator $ filter (\r -> 1==denominator r && numerator r > 0) $ mapMaybe rpn $ getExpressionList xs

getExpressionList :: [String] -> [[String]]
getExpressionList ns = loop [] oplist
    where
        oplist = [[o1,o2,o3] | o1 <- ["+","-","*","/"], o2 <- ["+","-","*","/"], o3 <- ["+","-","*","/"]]
        loop list [] = list
        loop list (x:xs) = loop (el'++list) xs
            where
                el' = map (\l -> take 2 ns ++ l ++ [last x]) (permutations $ init x++drop 2 ns)

rpn :: [String] -> Maybe Rational
rpn  = loop [] 
    where
        loop [] [] = Nothing
        loop (x:xs) []
            | null xs = Just x
            | otherwise = Nothing
        loop [] (c:cs) = loop [fromIntegral (read c)] cs
        loop [x] (c:cs)
            | isOp = Nothing
            | otherwise = loop (fromIntegral (read c):[x]) cs
            where
                isOp = c == "+" || c == "-" || c == "*" || c == "/"
        loop p@(x:y:ys) (c:cs)
            | c == "+" = loop ((y + x):ys) cs
            | c == "-" = loop ((y - x):ys) cs
            | c == "*" = loop ((y * x):ys) cs
            | c == "/" = if x /= 0 then loop ((y / x):ys) cs else Nothing
            | otherwise = loop (fromIntegral (read c):p) cs

comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs
