import Data.List

main :: IO ()
main = do
    list <- readWordList
    print $ length $ problem42 list

problem42 :: [String] -> [Int]
problem42 l = filter (flip elem triangleList) lengthList
    where
        lengthList = map makeScore l

readWordList :: IO [String]
readWordList = do
    names <- readFile "words.txt"
    let nameList = sort $ words $ map (\x -> if x == ',' then ' ' else x) $ filter (/='"') names
    return nameList

makeScore :: String -> Int
makeScore t = foldl (\acc x -> acc + getScore x) 0 t
    where
        getScore x = fromEnum x - fromEnum 'A' + 1

triangleList :: [Int]
triangleList = map (\n -> n*(n+1)`div`2) [1..100]
