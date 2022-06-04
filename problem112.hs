import Data.List
import Data.Ratio

main :: IO ()
main = do
    print $ problem112

problem112 :: Integer
problem112 = loop 0 1
    where
        loop :: Integer -> Integer -> Integer
        loop cnt i
            | cnt % i == 99%100 = i
            | isUpNum i || isDownNum i = loop cnt (i+1)
            | otherwise = loop (cnt+1) (i+1)

isUpNum :: Integer -> Bool
isUpNum n = let t = show n in sort t == t

isDownNum :: Integer -> Bool
isDownNum n = let t = show n in sortBy (flip compare) t == t
