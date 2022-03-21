import Data.List

main :: IO ()
main = do
    print $ problem26

problem26 :: Maybe Int
problem26 = let x = [getLength x | x <- [1..1000-1]]
            in (+1) <$> findIndex (==maximum x) x

maxDigit :: Integer
maxDigit = 5000

getLength :: Integer -> Integer
getLength n
    | (1*10^maxDigit) `mod` n == 0 = 0
    | otherwise = loop a (tail a) 1
    where
        a = show $ div (1*10^maxDigit) n 
        loop :: String -> String -> Integer -> Integer
        loop [] [] _ = 0
        loop (x:xs) [] _ = loop xs (tail xs) 1
        loop p@(x:xs) q@(y:ys) l
            | x == y && length q < 100 = loop xs (tail xs) 1
            | x == y = if checkMatch p q then l else loop p ys (l+1)
            | otherwise = loop p ys (l+1)

checkMatch :: [Char] -> [Char] -> Bool
checkMatch [] _ = True
checkMatch _ [] = True 
checkMatch (x:xs) (y:ys) 
    | x == y = checkMatch xs ys
    | otherwise = False 
