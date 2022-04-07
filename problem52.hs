import Data.List

main :: IO ()
main = do
    print $ problem52

problem52 :: [Integer]
problem52 = loop [[10..100`div`6],[100..1000`div`6],[1000..10000`div`6],[10000..100000`div`6],[100000..1000000`div`6]]
    where
        loop [] = []
        loop (x:xs) = filter isContainSameNum x ++ loop xs

isContainSameNum :: Integer -> Bool
isContainSameNum n = all (isSame n) [2*n,3*n..6*n]
    where
        isSame x y = null (show x \\ show y)
