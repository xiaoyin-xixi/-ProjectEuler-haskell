
main :: IO ()
main = do
    list <- readTriangleList
    print $ problem67 $ reverse list

readTriangleList :: IO [[Integer]]
readTriangleList = do
    list <- readFile "triangle.txt"
    return $ loop $ map words $ lines list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Integer) x : loop xs


selMaxSum :: [Integer] -> [Integer] -> [Integer]
selMaxSum _ [] = []
selMaxSum [] _ = []
selMaxSum p@(x:y:xs) (z:zs) = [max (z+x) (z+y)] ++ selMaxSum (tail p) zs

problem67 :: [[Integer]] -> [Integer]
problem67 (n:ns) = loop ns n
    where
        loop :: [[Integer]] -> [Integer] -> [Integer]
        loop _ [] = []
        loop [] y = y
        loop (x:xs) y = loop xs $ selMaxSum y x
