import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.List
main :: IO ()
main = do
    print $ sum $ nub $ drop 1 $ elems problem88

limit :: Int
limit = 12000

problem88 :: Array Int Int
problem88 = runSTArray $ do
    arr <- newListArray (1, limit) (repeat (maxBound :: Int))
    productSum arr [] 2
    return arr
    where
        productSum :: STArray s Int Int -> [Int] -> Int -> ST s ()
        productSum arr facList f0 = do
            loop arr f0
            return ()
            where
                loop :: STArray s Int Int -> Int -> ST s ()
                loop arr f
                    | k > limit = return ()
                    | fn == 1 && p*f>limit*2 = return ()
                    | otherwise = do
                        when (fn>=2) $ do
                            k' <- readArray arr k
                            when (p<k') $ writeArray arr k p
                        productSum arr list f
                        loop arr (f+1)
                    where
                        list = f:facList
                        p = product list
                        s = sum list
                        fn = length list
                        k = p - s + fn
