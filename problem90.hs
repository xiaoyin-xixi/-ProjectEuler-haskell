import Data.List

main :: IO ()
main = do
    print $ problem90

problem90 :: Int
problem90 = length $ filter (==True) $ map checkSquare $ pairList $ makeCubeList $ comb 6 [0,1,2,3,4,5,6,7,8,9]
    where
        pairList [] = []
        pairList (x:xs) = [(x,y) | y <- xs] ++ pairList xs

checkSquare :: ([Int], [Int]) -> Bool
checkSquare (xs,ys) = all (check xs ys) squareList
    where
        squareList = ["01","04","09","16","25","36","49","64","81"]
        check :: [Int] -> [Int] -> [Char] -> Bool
        check xs ys sq
            | d10 `elem` xs && d1 `elem` ys = True
            | d10 `elem` ys && d1 `elem` xs = True
            | otherwise = False
            where
                d10 = read [sq!!0]
                d1 = read [sq!!1]

makeCubeList :: [[Int]] -> [[Int]]
makeCubeList list = loop list []
    where
        loop [] result = result
        loop (x:xs) result = loop xs (sort (adding6or9 x):result)

adding6or9 :: [Int] -> [Int]
adding6or9 xs
    | 6 `elem` xs && 9 `notElem` xs = 9:xs
    | 9 `elem` xs && 6 `notElem` xs = 6:xs
    | otherwise = xs

comb :: Int -> [Int] -> [[Int]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs
