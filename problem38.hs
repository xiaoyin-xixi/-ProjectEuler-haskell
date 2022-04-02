import Data.List

main :: IO ()
main = do
    print $ maximum problem38

problem38 :: [Integer]
problem38 = [read t | n <- [1..9999], x <- [2..9], let t = makeNumText n [1..x], isPandigital t ]

makeNumText :: Integer -> [Integer] -> String
makeNumText n xs = foldl (\acc x -> acc ++ show (n*x)) "" xs

isPandigital :: String -> Bool
isPandigital s
    | find (=='0') s /= Nothing = False
    | len == len' = if len == 9 then True else False 
    | otherwise = False
    where
        len = length s
        len' = length $ nub s
