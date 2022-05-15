import Data.List

main :: IO ()
main = do
    list <- readWordList
    print $ maximum $ problem98 (makeAnagramPairs list) makeAnagramSquares

problem98 :: [[[String]]] -> [[[String]]] -> [Int]
problem98 anaWords anaSquares = loop 0 []
    where
        loop 9 list = list
        loop cnt list = loop (cnt+1) (vlist ++ list)
            where
                vlist = loop' w n []
                n = anaSquares!!cnt
                w = anaWords!!cnt
        loop' :: [[String]] -> [[String]] -> [Int] -> [Int]
        loop' [] _ list' = list'
        loop' (w:ws) ns list' = loop' ws ns (maxValues ++ list')
            where
                maxValues = loop'' w ns list'
                loop'' :: [String] -> [[String]] -> [Int] -> [Int]
                loop'' _ [] list'' = list''
                loop'' words (n:ns) list'' =
                    case maxValue of
                        Just x -> loop'' words ns (x:list'')
                        _ -> loop'' words ns list''
                    where
                        maxValue = checkAnagramSquare words n

checkAnagramSquare :: [String] -> [String] -> Maybe Int
checkAnagramSquare words nums =
    case match of
        Just m -> convertLoop (tail words) m [num]
            where
                num = convertToNum w' m
        _ -> Nothing
    where
        convertLoop [] _ list
            | length list == 1 = Nothing
            | otherwise = Just (maxValue list)
            where
                maxValue l = maximum $ map read l
        convertLoop (w:ws) maps list
            | num `elem` nums = convertLoop ws maps (num:list)
            | otherwise = convertLoop ws maps list
            where
                num = convertToNum w maps
        w' = head words
        match = matchingLoop w' nums
        matchingLoop _ [] = Nothing
        matchingLoop word (n:ns) =
            case tryMatching word n of
                Just list -> Just list
                _ -> matchingLoop word ns

convertToNum :: String -> [(Char,Char)] -> String
convertToNum word maps = loop word []
    where
        loop [] num = num
        loop (w:ws) num = 
            case lookup w maps of
                Just x -> loop ws (num ++ [x])
                _ -> ""

tryMatching :: String -> String -> Maybe [(Char,Char)]
tryMatching word num
    | not isSameLength = Nothing
    | otherwise = loop word num [] []
    where
        loop [] [] list _ = Just list
        loop [] _ _ _ = Nothing
        loop _ [] _ _ = Nothing
        loop (w:ws) (n:ns) list used = 
            case lookup w list of
                Just x -> if x == n then loop ws ns list used
                            else Nothing
                _ -> if n `elem` used then Nothing else loop ws ns ((w,n):list) (n:used)
        isSameLength = length word == length num

makeAnagramSquares :: [[[String]]]
makeAnagramSquares = group' 9 []
    where
        group' 0 list = list
        group' cnt list = group' (cnt-1) [sameGroup] ++ list
            where
                sameGroup = [[a | (a,_) <- pairs] | pairs <- filter (\p -> length (fst (head p)) == cnt) pairsList]
        pairsList = filter (\a -> length a > 1) $ groupBy (\(_,a) (_,b) -> a == b) $ 
                    sortBy (\(_,a) (_,b) -> compare a b) $ map (\x -> (show x, sort $ show x)) $ takeWhile (<1000000000) [x^2 | x <- [1..]]

makeAnagramPairs ::[String] -> [[[String]]]
makeAnagramPairs list = group' 9 []
    where
        group' 0 l = l
        group' cnt l = group' (cnt-1) [sameGroup] ++ l
            where
                sameGroup = [[a | (a,_) <- pairs] | pairs <- filter (\p -> length (fst (head p)) == cnt) pairsList]
        pairsList = filter (\a -> length a > 1) $ groupBy (\(_,a) (_,b) -> a == b) $ sortBy (\(_,a) (_,b) -> compare a b) [(word, sort word) | word <- list]

maxAnagramLength :: Foldable t => [[t a]] -> Int
maxAnagramLength list = maximum $ map (length . head) list

readWordList :: IO [String]
readWordList = do
    names <- readFile "p098_words.txt"
    let nameList = words $ map (\x -> if x == ',' then ' ' else x) $ filter (/='"') names
    return nameList
