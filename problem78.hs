import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST

main :: IO ()
main = do
    print $ problem78

problem78 :: Maybe Int
problem78 = findIndex (\cnt -> cnt `mod` 1000000 == 0) $ elems $ runSTArray $ countExpressions 100000

countExpressions :: Int -> ST s (STArray s Int Integer)
countExpressions n = do
    p <- pArray
    foldM (\acc m -> loop m [0..] acc) p [1..n+1]
    where
        pArray = newListArray (0,limit+1) $ 1:replicate (limit + 1) 0
        loop _ [] arr = return arr
        loop n (k:ks) arr
            | n<mp = return arr
            | n<mm = do
                update' arr sign mp
                return arr
            | otherwise = do
                update arr sign mp mm
                loop n ks arr
            where
                mp = k * (3*k-1) `div` 2
                mm = k * (3*k+1) `div` 2
                sign = if k`mod`2 == 1 then 1 else -1
                update arr sign mp mm = do
                    addp <- readArray arr (n-mp)
                    addm <- readArray arr (n-mm)
                    org <- readArray arr n
                    writeArray arr n (org + sign * (addp + addm))
                update' arr sign mp = do
                    addp <- readArray arr (n-mp)
                    org <- readArray arr n
                    writeArray arr n (org + sign * addp)
limit :: Int
limit = 100000
