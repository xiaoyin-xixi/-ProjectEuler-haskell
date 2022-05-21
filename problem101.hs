import Data.Ratio
import Data.List

main :: IO ()
main = do
    print $ problem101

problem101 :: Integer
problem101 = numerator $ foldl (\acc n -> acc + op n (fromIntegral (n+1))) 1 [2..10]

op :: Int -> Rational -> Rational
op k = polynominal factors
    where
        mat = map (take k) $ take k matrix
        u = take k uList
        factors = gauss mat u

gauss :: [[Rational]] -> [Rational] -> [Rational]
gauss a b = backwardSubstitution $ forwardElimination ab []
    where
        ab = zipWith (\a b -> a++[b]) a b

forwardElimination :: [[Rational]] -> [[Rational]] -> [[Rational]]
forwardElimination input list
    | length input == 1 = list ++ input
    | otherwise = forwardElimination (loop (head i') (tail i') []) (list ++ [head i'])
    where
        i' = sortBy (\a b -> compare (head b ^2) (head a ^2)) input
        loop _ [] list = list
        loop base (r:rs) list = loop base rs (list++[l])
            where
                d = head r / head base
                l = drop 1 $ zipWith (\a b -> b - d*a) base r

backwardSubstitution :: [[Rational]] -> [Rational]
backwardSubstitution input = loop (reverse input) [-1]
    where
        loop [] ans = init ans
        loop (a:as) ans = loop as (a':ans)
            where
                ttl = sum $ zipWith (*) (reverse a) (reverse ans)
                a' = - ttl / head a

matrix :: [[Rational]]
matrix = [[n^e | e <- [0,1,2,3,4,5,6,7,8,9,10]] | n <- [1,2,3,4,5,6,7,8,9,10,11]]

uList :: [Rational]
uList = map (polynominal [1,-1,1,-1,1,-1,1,-1,1,-1,1]) [1..11]

polynominal :: Num a => [a] -> a -> a
polynominal factors n = sum $ zipWith (*) factors $ expList n
    where
        len = length factors
        expList n = take len $ 1:iterate (*n) n
