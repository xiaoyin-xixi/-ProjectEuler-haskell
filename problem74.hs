import Data.Array
import Data.Char

main :: IO ()
main = do
    print $ problem74

problem74 :: Int
problem74 = length $ filter (==60) $ map (\n -> loop n [n] 1) [1..999999]
    where
        fracArray = listArray (0,9) $ scanl (*) 1 [1..9] 
        frac n = fracArray!n
        limit = 6 * frac 9
        fracSum n = foldl (\acc c -> acc + frac (digitToInt c)) 0 $ show n
        mapList = map fracSum [0..limit]
        mapArray = listArray (0,limit) mapList
        loop n l cnt
            | next `elem` l = cnt
            | otherwise = loop next (next:l) (cnt+1)
                where
                    next = mapArray!n
