main :: IO ()
main = do
    print $ problem6 100

problem6 :: Integer -> Integer
problem6 n = let
    squareSum = sum [x^2 | x <- [1..n]]
    sumSquare = (sum [1..n])^2
    in sumSquare - squareSum
