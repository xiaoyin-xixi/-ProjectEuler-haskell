import Data.List

main :: IO ()
main = do
    print $ foldl (\acc x -> acc ++ show x) "" $ (sort $ permutations [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) !! (10^6-1)

