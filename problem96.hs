import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import GHC.Arr

main :: IO ()
main = do
    list <- readSudokuList
    print $ sum $ loop [] list
    where
        loop :: [Int] -> [[((Int, Int), Int)]] -> [Int]
        loop acc [] = acc
        loop acc (s:ss) = loop (acc ++ [num]) ss
            where
                three = take 3 $ concat $ elems $ problem96 s
                num = read $ concatMap show three

problem96 :: [((Int, Int), Int)] -> Array Int [Int]
problem96 sudokList = runSTArray $ do
    candidateArr <- newListArray (1, 9*9) (repeat [1,2,3,4,5,6,7,8,9])
    cDecidedArr <- newListArray (1, 9) (repeat [])
    rDecidedArr <- newListArray (1, 9) (repeat [])
    sDecidedArr <- newListArray (1, 9) (repeat [])
    setDecidedList cDecidedArr rDecidedArr sDecidedArr candidateArr sudokList
    update cDecidedArr rDecidedArr sDecidedArr candidateArr
    cl <- getCandidateList candidateArr
    cArr' <- freeze cDecidedArr
    rArr' <- freeze rDecidedArr
    sArr' <- freeze sDecidedArr
    canArr' <- freeze candidateArr
    let res = solver cArr' rArr' sArr' canArr' cl
    thawSTArray res
    where
        checkFailure :: STArray s Int [Int] -> ST s Bool
        checkFailure canArr = do 
            list <- getElems canArr
            return $ any null list
        isTerminate canArr = do
            list <- getCandidateList canArr
            return (length list == 9)
        getCandidateList :: STArray s Int [Int] -> ST s [((Int, Int), Int)]
        getCandidateList canArr = do
            minRef <- newSTRef ((1,1),[1,2,3,4,5,6,7,8,9])
            forM_ [1..9] $ \x -> do
                forM_ [1..9] $ \y -> do
                    v <- readArray canArr (9*(y-1) + x)
                    when (length v /= 1) $ do
                        min' <- readSTRef minRef
                        when (length v < length (snd min')) $  modifySTRef' minRef (const ((x,y),v))
            candidate <- readSTRef minRef
            listRef <- newSTRef []
            forM_ (snd candidate) $ \l -> do
                modifySTRef' listRef (\list -> (fst candidate, l):list)
            readSTRef listRef

        solver :: Array Int [Int] -> Array Int [Int] -> Array Int [Int] -> Array Int [Int] -> [((Int, Int), Int)] -> Array Int [Int]
        solver cArr rArr sArr canArr [] = canArr
        solver cArr rArr sArr canArr (c:cs) = runST $ do
            resRef <- newSTRef canArr
            cST <- thaw cArr
            rST <- thaw rArr
            sST <- thaw sArr
            canST <- thaw canArr
            isTerm <- isTerminate canST
            unless isTerm $ do
                setDecidedList cST rST sST canST [c]
                failure <- checkFailure canST
                unless failure $ update cST rST sST canST
                failure <- checkFailure canST
                if failure then do
                    let res = solver cArr rArr sArr canArr cs
                    modifySTRef' resRef (const res)
                else do
                    cl <- getCandidateList canST
                    if length cl<9 then do
                        cArr' <- freeze cST
                        rArr' <- freeze rST
                        sArr' <- freeze sST
                        canArr' <- freeze canST
                        let res = solver cArr' rArr' sArr' canArr' cl
                        modifySTRef' resRef (const res)
                        resArr <- thaw res
                        isTerm <- isTerminate resArr 
                        unless isTerm $ do
                            let res = solver cArr rArr sArr canArr cs
                            modifySTRef' resRef (const res)
                    else do
                        res <- freeze canST
                        modifySTRef' resRef (const res)
            readSTRef resRef

        setDecidedList :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> [((Int, Int), Int)] -> ST s ()
        setDecidedList cArr rArr sArr canArr decidingList = do
            refBool <- newSTRef True
            forM_ decidingList $ \((x,y),v) -> do
                c <- readArray cArr x
                r <- readArray rArr y
                let x' = (x-1)`div`3 + 1
                let y' = (y-1)`div`3 + 1
                s <- readArray sArr (3*(y'-1) + x')
                if v`elem`c || v`elem`r || v`elem`s then do
                        modifySTRef' refBool (const False)
                        writeArray canArr (9*(y-1) + x) []
                    else
                    writeArray canArr (9*(y-1) + x) [v]
                writeArray cArr x (nub (v:c))
                writeArray rArr y (nub (v:r))
                writeArray sArr (3*(y'-1) + x') (nub (v:s))
            b <- readSTRef refBool
            when b $ deleteCandidate cArr rArr sArr canArr

        deleteCandidate :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> ST s ()
        deleteCandidate cArr rArr sArr canArr = do
            refdeciding <- newSTRef []
            forM_ [1..9] $ \x -> do
                forM_ [1..9] $ \y -> do
                    v <- readArray canArr (9*(y-1) + x)
                    when (length v /= 1) $ do
                        c <- readArray cArr x
                        r <- readArray rArr y
                        let x' = (x-1)`div`3 + 1
                        let y' = (y-1)`div`3 + 1
                        s <- readArray sArr (3*(y'-1) + x')
                        let v' = ((v \\ c) \\ r) \\s
                        when (length v' == 1) $ modifySTRef' refdeciding (\l -> ((x,y),head v'):l)
                        writeArray canArr (9*(y-1) + x) v'
            r <- readSTRef refdeciding
            unless (null r) $ do setDecidedList cArr rArr sArr canArr r
        update :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> ST s ()
        update cArr rArr sArr canArr = do
            setDecidedList cArr rArr sArr canArr =<< chooseCandidatesFromSquareInfo canArr
            setDecidedList cArr rArr sArr canArr =<< chooseCandidatesFromColumnInfo canArr
            setDecidedList cArr rArr sArr canArr =<< chooseCandidatesFromRowInfo canArr
        chooseCandidatesFromSquareInfo :: STArray s Int [Int] -> ST s [((Int, Int), Int)]
        chooseCandidatesFromSquareInfo canArr = getCandidates canArr [1,2,3,10,11,12,19,20,21] [0,3,6,27,30,33,54,57,60]
        chooseCandidatesFromRowInfo :: STArray s Int [Int] -> ST s [((Int, Int), Int)]
        chooseCandidatesFromRowInfo canArr = getCandidates canArr [1,2,3,4,5,6,7,8,9] [0,9,18,27,36,45,54,63,72]
        chooseCandidatesFromColumnInfo :: STArray s Int [Int] -> ST s [((Int, Int), Int)]
        chooseCandidatesFromColumnInfo canArr = getCandidates canArr [1,10,19,28,37,46,55,64,73] [0,1,2,3,4,5,6,7,8]
        getCandidates :: STArray s Int [Int] -> [Int] -> [Int] -> ST s [((Int, Int), Int)]
        getCandidates canArr ns ms = do
            refdeciding <- newSTRef []
            forM_ ms $ \m -> do
                refque <- newSTRef []
                forM_ ns $ \n -> do
                    let p = m + n
                    v <- readArray canArr p
                    when (length v /= 1) $ modifySTRef' refque (v++)
                que <- readSTRef refque
                let a = concat $ filter (\l -> length l == 1) $ group $ sort que
                unless (null a) $ do
                    forM_ a $ \q -> do
                        forM_ ns $ \n -> do
                            let p = m + n
                                x = (p-1) `mod` 9 + 1
                                y = (p-1) `div` 9 + 1
                            v <- readArray canArr p
                            when (q `elem` v) $ modifySTRef' refdeciding (\l -> ((x,y),q):l)
            readSTRef refdeciding

readSudokuList :: IO [[((Int, Int), Int)]]
readSudokuList = do
    list <- readFile "p096_sudoku.txt"
    let t = lines list
    let l = loop' [] t
    return l
    where
        loop' :: [[((Int,Int),Int)]] -> [String] -> [[((Int,Int),Int)]]
        loop' l [] = l
        loop' l text = loop' (loop [] 1 (tail . take 10 $ text) : l) $ drop 10 text
            where
                loop :: [((Int,Int),Int)] -> Int -> [String] -> [((Int,Int),Int)]
                loop l y [] = l
                loop l y (x:xs) = loop (makeList l 1 x) (y+1) xs
                    where
                        makeList :: [((Int,Int),Int)] -> Int -> [Char] -> [((Int,Int),Int)]
                        makeList l x [] = l
                        makeList l x (c:cs) = if c /= '0' then makeList (((x,y),read [c]):l) (x+1) cs else makeList l (x+1) cs 
