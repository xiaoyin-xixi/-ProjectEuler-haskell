import Data.List
import Data.Maybe

main :: IO ()
main = do
    pList <- readPassPhraseList
    print $ problem79 pList [100..]

problem79 :: [String] -> [Integer] -> Maybe Integer
problem79 _ [] = Nothing
problem79 pList (n:ns)
    | isNothing chk = problem79 pList ns
    | otherwise = chk
    where
        chk = check n pList
        check n [] = Just n
        check n (x:xs)
            | isSubsequenceOf x text = check n xs
            | otherwise = Nothing
            where
                text = show n

readPassPhraseList :: IO [String]
readPassPhraseList = do
    list <- readFile "p079_keylog.txt"
    return $ lines list
