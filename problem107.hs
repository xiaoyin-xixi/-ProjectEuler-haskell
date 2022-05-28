import Data.List.Split

main :: IO ()
main = do
    list <- readMatrixList
    let total = sum (map sum list) `div` 2
    let minSum = foldl (\acc (_,n) -> acc + n) 0 $ getMinPoint list [0] []
    print $ total - minSum

getMinPoint :: [[Int]] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
getMinPoint matrix decidedPoints que
    | length decidedPoints == 40 = que
    | otherwise = getMinPoint matrix (fst minPoint:decidedPoints) (minPoint:que)
    where
        minPoint = loop decidedPoints (0,maxBound::Int)
        loop [] mp = mp
        loop (dp:dps) mp = loop dps minPoint'
            where
                minPoint' = loop' dp decidedPoints 0 (snd mp) (fst mp)
                loop' _ _ 40 minValue point = (point, minValue)
                loop' dp dps cnt minValue point
                    | d == 0 = loop' dp dps (cnt+1) minValue point
                    | cnt `elem` dps = loop' dp dps (cnt+1) minValue point
                    | d < minValue = loop' dp dps (cnt+1) d cnt
                    | otherwise = loop' dp dps (cnt+1) minValue point
                    where
                        d = (matrix!!dp)!!cnt

readMatrixList :: IO [[Int]]
readMatrixList = do
    set <- readFile "p107_network.txt"
    let list = map (splitOn ",") $ lines set
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> if n == "-" then 0 else read n :: Int) x : loop xs
