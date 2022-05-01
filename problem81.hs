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
    print $ problem81 list

problem81 :: [[Int]] -> Int
problem81 list = dijkstraAlg list ! (80*80-1)

dijkstraAlg :: [[Int]] -> Array Int Int
dijkstraAlg matrix = runSTArray $ do
    let s = head (head matrix)
    weiArr <- newListArray (0, 80*80) (s : repeat (maxBound :: Int))
    loop weiArr [(s,(1,1))]
    return weiArr
        where
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
                    pints (x,y) = 80*(y-1) + x-1
                    get (x,y) = (matrix !! (y-1)) !! (x-1)
                    path (80,80) = []
                    path (80,y) = let tp = (80,y+1) in [(tp, get tp)]
                    path (x,80) = let tp = (x+1,80) in [(tp, get tp)]
                    path (x,y) = let tp1 = (x+1,y)
                                     tp2 = (x,y+1)
                                     in [(tp1, get tp1),(tp2, get tp2)]

readMatrixList :: IO [[Int]]
readMatrixList = do
    matrix <- readFile "p081_matrix.txt"
    let list = map (splitOn ",") $ lines matrix
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Int) x : loop xs
