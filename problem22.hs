import Data.List

main :: IO ()
main = do
    list <- readNameList
    print $ problem22 1 list


readNameList :: IO [String]
readNameList = do
    names <- readFile "p022_names.txt"
    let nameList = sort $ words $ map (\x -> if x == ',' then ' ' else x) $ filter (/='"') names
    return nameList

makeScore :: String -> Int
makeScore t = foldl (\acc x -> acc + getScore x) 0 t
    where
        getScore x = fromEnum x - fromEnum 'A' + 1

problem22 :: Int -> [String] -> Int
problem22 _ [] = 0
problem22 n (x:xs) = n * makeScore x + problem22 (n+1) xs
