import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import Debug.Trace
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
    refque <- newSTRef sudokList
    que <- readSTRef refque
    setDecidedList cDecidedArr rDecidedArr sDecidedArr candidateArr que
    setCandidate cDecidedArr rDecidedArr sDecidedArr candidateArr
    setCandidate' cDecidedArr rDecidedArr sDecidedArr candidateArr
    setCandidate'' cDecidedArr rDecidedArr sDecidedArr candidateArr
    cl <- getCandidateList candidateArr
    cArr' <- freeze cDecidedArr
    rArr' <- freeze rDecidedArr
    sArr' <- freeze sDecidedArr
    canArr' <- freeze candidateArr
--    trace ("initial:"++ show cl) $ pure ()
    let res = solver cArr' rArr' sArr' canArr' cl
    thawSTArray res
    where
        check :: STArray s Int [Int] -> ST s Bool
        check canArr = do 
            list <- getElems canArr
            return $ any null list
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
--            trace (show canArr) $ pure ()
            resRef <- newSTRef canArr
            cST <- thaw cArr
            rST <- thaw rArr
            sST <- thaw sArr
            canST <- thaw canArr
            cList <- getCandidateList canST
            when (length cList<9) $ do
                setDecidedList cST rST sST canST [c]
                failure <- check canST
                unless failure $ do
                    setCandidate cST rST sST canST
                    setCandidate' cST rST sST canST
                    setCandidate'' cST rST sST canST
                failure <- check canST
                if failure then do
--                    trace ("failure:"++ show cs) $ pure ()
                    let res = solver cArr rArr sArr canArr cs
                    modifySTRef' resRef (const res)
                else do
                    cl <- getCandidateList canST
--                    trace ("success:"++ show cl) $ pure ()
                    if length cl<9 then do
                        cArr' <- freeze cST
                        rArr' <- freeze rST
                        sArr' <- freeze sST
                        canArr' <- freeze canST
                        let res = solver cArr' rArr' sArr' canArr' cl
                        modifySTRef' resRef (const res)
                        resArr <- thaw res
                        cl' <- getCandidateList resArr 
                        when (length cl'<9) $ do
--                            trace ("repeat:"++ show cs) $ pure ()
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
        setCandidate :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> ST s ()
        setCandidate cArr rArr sArr canArr = do
            refdeciding <- newSTRef []
            forM_ [1..9] $ \n -> do
                refs <- setsCandidate canArr n
                s <- readSTRef refs
                modifySTRef' refdeciding (s++)
            q <- readSTRef refdeciding
            unless (null q) $ setDecidedList cArr rArr sArr canArr q
        setCandidate' :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> ST s ()
        setCandidate' cArr rArr sArr canArr = do
            refdeciding <- newSTRef []
            forM_ [1..9] $ \n -> do
                refs <- setcCandidate canArr n
                s <- readSTRef refs
                modifySTRef' refdeciding (s++)
            q <- readSTRef refdeciding
            unless (null q) $ setDecidedList cArr rArr sArr canArr q
        setCandidate'' :: STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> STArray s Int [Int] -> ST s ()
        setCandidate'' cArr rArr sArr canArr = do
            refdeciding <- newSTRef []
            forM_ [1..9] $ \n -> do
                refs <- setrCandidate canArr n
                s <- readSTRef refs
                modifySTRef' refdeciding (s++)
            q <- readSTRef refdeciding
            unless (null q) $ setDecidedList cArr rArr sArr canArr q

        setsCandidate :: STArray s Int [Int] ->  Int -> ST s (STRef s [((Int, Int), Int)])
        setsCandidate canArr n = do
            refdeciding <- newSTRef []
            refque <- newSTRef []
            forM_ [1..3] $ \i -> do
                let x = ((n-1)`mod`3) * 3 + i
                forM_ [1..3] $ \j -> do
                    let y = ((n-1)`div`3) * 3 + j
                    v <- readArray canArr (9*(y-1) + x)
                    when (length v /= 1) $ modifySTRef' refque (v++)
            que <- readSTRef refque
            let a = concat $ filter (\l -> length l == 1) $ group $ sort que
            unless (null a) $ do
                forM_ a $ \q -> do
                    forM_ [1..3] $ \i -> do
                        let x = ((n-1)`mod`3) * 3 + i
                        forM_ [1..3] $ \j -> do
                            let y = ((n-1)`div`3) * 3 + j
                            v <- readArray canArr (9*(y-1) + x)
                            when (q `elem` v) $ do
                                modifySTRef' refdeciding (\l -> ((x,y),q):l)
                                writeArray canArr (9*(y-1) + x) [q]
            return refdeciding
        setcCandidate :: STArray s Int [Int] -> Int -> ST s (STRef s [((Int, Int), Int)])
        setcCandidate canArr n = do
            refdeciding <- newSTRef []
            refque <- newSTRef []
            let x = n
            forM_ [1..9] $ \y -> do
                v <- readArray canArr (9*(y-1) + x)
                when (length v /= 1) $ modifySTRef' refque (v++)
            que <- readSTRef refque
            let a = concat $ filter (\l -> length l == 1) $ group $ sort que
            unless (null a) $ do
                forM_ a $ \q -> do
                    forM_ [1..9] $ \y -> do
                        v <- readArray canArr (9*(y-1) + x)
                        when (q `elem` v) $ do
                            modifySTRef' refdeciding (\l -> ((x,y),q):l)
                            writeArray canArr (9*(y-1) + x) [q]
            return refdeciding
        setrCandidate :: STArray s Int [Int] -> Int -> ST s (STRef s [((Int, Int), Int)])
        setrCandidate canArr n = do
            refdeciding <- newSTRef []
            refque <- newSTRef []
            let y = n
            forM_ [1..9] $ \x -> do
                v <- readArray canArr (9*(y-1) + x)
                when (length v /= 1) $ modifySTRef' refque (v++)
            que <- readSTRef refque
            let a = concat $ filter (\l -> length l == 1) $ group $ sort que
            unless (null a) $ do
                forM_ a $ \q -> do
                    forM_ [1..9] $ \x -> do
                        v <- readArray canArr (9*(y-1) + x)
                        when (q `elem` v) $ do
                            modifySTRef' refdeciding (\l -> ((x,y),q):l)
                            writeArray canArr (9*(y-1) + x) [q]
            return refdeciding

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
