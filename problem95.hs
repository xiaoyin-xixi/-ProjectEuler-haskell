import Data.List
import Data.Array

main :: IO ()
main = do
    print $ problem95

limit = 1000000

problem95 :: Maybe Int
problem95 = minimum . chain [] . (+1) <$> elemIndex (maximum cntList) cntList
    where
        mapList = map (\n -> let s = sum $ divisors n in if s<limit then s else 0) [1..limit-1]
        mapArray = listArray (1,limit) mapList
        cntList = map (\n -> loop n [n] 1) [1..limit-1] 
        loop n l cnt
            | next == 0 = 0
            | next `elem` l = if next == last l then cnt else 0
            | otherwise = loop next (next:l) (cnt+1)
                where
                    next = mapArray!n
        chain l n
            | next `elem` l = l
            | otherwise = chain (next:l) next
                where
                    next = mapArray!n

divisors :: Int -> [Int]
divisors n = delete n $ loop [1..isqrt n]
    where
        isqrt = floor . sqrt . fromIntegral
        loop :: [Int] -> [Int]
        loop [] = []
        loop (m:ms) =
            let ds = case n `mod` m of
                        0 -> if m*m == n then [m] else [m, div n m]
                        otherwisw -> []
            in ds ++ loop ms
