import Data.List
import Data.STRef
import Data.List.Split
import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

main :: IO ()
main = do
    list <- readMatrixList
    print $ problem82 list

problem82 :: [[Int]] -> Int
problem82 list = minimum $ map (min . arrW) [1..80]
    where
        getW arr (x,y) = arr ! (80*(y-1) + x-1)
        arrW y = dijkstraAlg list (1,y)
        min ary = minimum $ map (\y -> getW ary (80,y)) [1..80]

dijkstraAlg :: [[Int]] -> (Int,Int) -> Array Int Int
dijkstraAlg matrix start = runSTArray $ do
    weiArr <- newListArray (0, 80*80) (repeat (maxBound :: Int))
    writeArray weiArr (pints start) $ get start
    loop weiArr [(get start, start)]
    return weiArr
        where
            pints (x,y) = 80*(y-1) + x-1
            get (x,y) = (matrix !! (y-1)) !! (x-1)
            loop :: STArray s Int Int -> [(Int,(Int,Int))] -> ST s ()
            loop arr que
                | null que = return ()
                | otherwise = do
                    refque <- newSTRef qs
                    w <- readArray arr $ pints p
                    when (d <= w) $ do
                        forM_ (path p) $ \(p', w') -> do
                            tw <- readArray arr (pints p')
                            when (w+w' < tw) $ do
                                writeArray arr (pints p') (w+w')
                                modifySTRef' refque (\que -> (w+w',p'):que)
                    loop arr =<< readSTRef refque
                where
                    ((d,p):qs) = sortOn fst que
                    path (80,_) = []
                    path (x,1)  = let tp1 = (x+1,1)
                                      tp2 = (x,2)
                                      in [(tp1, get tp1),(tp2, get tp2)]
                    path (x,80) = let tp1 = (x+1,80)
                                      tp2 = (x,80-1)
                                      in [(tp1, get tp1),(tp2, get tp2)]
                    path (x,y)  = let tp1 = (x+1,y)
                                      tp2 = (x,y+1)
                                      tp3 = (x,y-1)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3)]

readMatrixList :: IO [[Int]]
readMatrixList = do
    matrix <- readFile "p082_matrix.txt"
    let list = map (splitOn ",") $ lines matrix
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Int) x : loop xs
