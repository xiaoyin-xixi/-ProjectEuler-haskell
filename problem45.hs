import Data.List

main :: IO ()
main = do
    print $ head $ dropWhile (<=40755) $ intersect hexagonalList $ intersect triangleList pentagonalList

triangleList :: [Integer]
triangleList = take 100000 $ map (\n -> n*(n+1) `div` 2) [1..]

pentagonalList :: [Integer]
pentagonalList = take 100000 $ map (\n -> n*(3*n-1) `div` 2) [1..]

hexagonalList :: [Integer]
hexagonalList = take 100000 $ map (\n -> n*(2*n-1)) [1..]
