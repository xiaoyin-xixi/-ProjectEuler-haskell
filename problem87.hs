import Data.Numbers.Primes
import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST

main :: IO ()
main = do
    print $ length $ filter (==True) $ elems problem87

problem87 :: Array Integer Bool
problem87 = runSTArray $ do
    arr <- newListArray (1, limit) (repeat False)
    loop arr fourthList
    return arr
    where
        loop arr [] = return ()
        loop arr (x:xs) = do
            loop' arr cubeList
            loop arr xs
            where
                loop' arr [] = return ()
                loop' arr (y:ys) = do
                    sumTriple arr squareList
                    loop' arr ys
                    where
                        sumTriple :: STArray s Integer Bool -> [Integer] -> ST s ()
                        sumTriple arr [] = return ()
                        sumTriple arr (z:zs) = do
                            when (x+y+z<limit) $ writeArray arr (x+y+z) True
                            sumTriple arr zs

limit :: Integer
limit = 50000000

squareList :: [Integer]
squareList = takeWhile (<limit) [x^2 | x <- primes] 

cubeList :: [Integer]
cubeList = takeWhile (<limit) [x^3 | x <- primes] 

fourthList :: [Integer]
fourthList = takeWhile (<limit) [x^4 | x <- primes]
