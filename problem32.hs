import Data.List

main :: IO ()
main = do
    print $ sum $ nub [a*b | a <- [1..1999], b <- [1..a], isPandigital a b (a*b)]


isPandigital :: Integer -> Integer -> Integer -> Bool
isPandigital a b c
    | find (=='0') s /= Nothing = False
    | len == len' = if len == 9 then True else False 
    | otherwise = False
    where
        s = show a ++ show b ++ show c
        len = length s
        len' = length $ nub s
