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
    print $ problem83 list

problem83 :: [[Int]] -> Int
problem83 list = getW (arrW 1) (80,80)
    where
        getW arr (x,y) = arr ! (80*(y-1) + x-1)
        arrW y = dijkstraAlg list (1,y)

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
                    path (80,80) = []
                    path (1,1)  = let tp1 = (2,1)
                                      tp2 = (1,2)
                                      in [(tp1, get tp1),(tp2, get tp2)]
                    path (80,1) = let tp1 = (79,1)
                                      tp2 = (80,2)
                                      in [(tp1, get tp1),(tp2, get tp2)]
                    path (1,80) = let tp1 = (1,79)
                                      tp2 = (2,80)
                                      in [(tp1, get tp1),(tp2, get tp2)]
                    path (x,1)  = let tp1 = (x+1,1)
                                      tp2 = (x,2)
                                      tp3 = (x-1,1)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3)]
                    path (x,80) = let tp1 = (x+1,80)
                                      tp2 = (x,80-1)
                                      tp3 = (x-1,80)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3)]
                    path (1,y)  = let tp1 = (2,y)
                                      tp2 = (1,y-1)
                                      tp3 = (1,y+1)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3)]
                    path (80,y) = let tp1 = (79,y)
                                      tp2 = (80,y-1)
                                      tp3 = (80,y+1)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3)]
                    path (x,y)  = let tp1 = (x+1,y)
                                      tp2 = (x,y+1)
                                      tp3 = (x,y-1)
                                      tp4 = (x-1,y)
                                      in [(tp1, get tp1),(tp2, get tp2),(tp3, get tp3),(tp4, get tp4)]

readMatrixList :: IO [[Int]]
readMatrixList = do
    matrix <- readFile "p082_matrix.txt"
    let list = map (splitOn ",") $ lines matrix
    return $ loop list
        where
            loop [] = []
            loop (x:xs) = map (\n -> read n :: Int) x : loop xs
