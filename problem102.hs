import Data.List.Split

main :: IO ()
main = do
    list <- readMatrixList
    print $ length $ filter problem102 list

problem102 :: [Integer] -> Bool
problem102 list = isInside (x1,y1) (x2,y2) (x3,y3)
    where
        x1 = list!!0
        y1 = list!!1
        x2 = list!!2
        y2 = list!!3
        x3 = list!!4
        y3 = list!!5

isInside :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Bool
isInside (x1,y1) (x2,y2) (x3,y3) = (c1>0 && c2>0 && c3>0) || (c1<0 && c2<0 && c3<0)
    where
        c1 = x1*y2 - y1*x2
        c2 = x2*y3 - y2*x3
        c3 = x3*y1 - y3*x1

readMatrixList :: IO [[Integer]]
readMatrixList = do
    matrix <- readFile "p102_triangles.txt"
    let list = map (splitOn ",") $ lines matrix
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Integer) x : loop xs
