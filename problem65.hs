import Data.Ratio
import Data.Char

main :: IO ()
main = do
    print $ foldl (\acc x -> acc + digitToInt x) 0 $ (show . numerator) $ problem65 $ reverse es

problem65 :: [Integer] -> Rational
problem65 (x:xs) = loop xs $ fromIntegral x
    where
        loop [] r = r
        loop (x:xs) r = loop xs d
            where
                d = fromIntegral x + (fromIntegral 1)/r 

es :: [Integer]
es = 2 : (take 99 $ loop 1)
    where
        loop i = [1,i*2,1] ++ loop (i+1)
