import Data.Array

main :: IO ()
main = do
    print $ problem23

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

divisors :: Integer -> [Integer]
divisors n = loop [1..isqrt n]
    where
        loop :: [Integer] -> [Integer]
        loop [] = []
        loop (m:ms) =
            let ds = case n `mod` m of
                        0 -> if m*m == n then [m] else [m, div n m]
                        otherwisw -> []
            in ds ++ loop ms

problem23 :: Integer
problem23 = foldl (\acc n -> acc + if isAbundantSum  n then 0 else n) 0 [1..28123-1]

abundantNumList :: [Integer]
abundantNumList = takeWhile (<28123) [x | x <- [2..], 2*x < (sum $ divisors x) ]

isAbundantSum :: Integer -> Bool
isAbundantSum n = any (\m -> (n-m) > 0 && isAbundantArray ! (n-m)) abundantNumList

isAbundantArray :: Array Integer Bool
isAbundantArray = listArray (1, 28123) [elem x abundantNumList | x <- [1..28123]]
