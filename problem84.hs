import Data.List
import System.Random
import Data.Array.IO
import Data.Ord

main :: IO ()
main = do
    a <- problem84
    print $ a

problem84 :: IO String
problem84 = do
    arr <- go
    list <- getElems arr
    let s = sortOn (Down . fst) $ zip list [drop 1 (show n) | n <- [100..139]]
    return (snd (s!!0) ++ snd (s!!1) ++ snd (s!!2))


data Dice = Dice {
    score :: Int, 
    doubleCnt :: Int
} deriving Show

data Board = Board {
    square :: Int,
    ccCard :: Int,
    chCard :: Int
} deriving Show

go :: IO (IOArray Int Int)
go = do
    cntArr <- newListArray (0, 39) (repeat 0)
    loop cntArr (Board 0 0 0) (Dice 0 0) [1..10000000]
    return cntArr
        where
            loop :: IOArray Int Int -> Board -> Dice -> [Int]-> IO ()
            loop arr _ _ [] = return ()
            loop arr bd di (x:xs) = do
                di' <- dice di
                let bd' = move bd di'
                let s = square bd'
                cur <- readArray arr s
                writeArray arr s (cur+1)
                loop arr bd' di' xs

dice :: Dice -> IO Dice
dice (Dice _ cnt) = do
    a <- randomRIO (1,max) :: IO Int
    b <- randomRIO (1,max) :: IO Int
    let cnt' = if a == b then (cnt+1)`mod`4 else 0
    return (Dice (a+b) cnt')
    where
        max = 4

move :: Board -> Dice -> Board
move bd dice
    | doubleCnt dice == 3 = bd {square = 10}
    | next == 2 = moveCC bd
    | next == 7 = moveCH bd
    | next == 17 = moveCC bd
    | next == 22 = moveCH bd
    | next == 30 = bd {square = 10}
    | next == 33 = moveCC bd
    | next == 36 = moveCH bd
    | otherwise = bd {square = next}
    where
        next = (square bd + score dice) `mod` 40
        moveCC bd
            | ccCard bd == 0 = bd' {square = 0}
            | ccCard bd == 1 = bd' {square = 10}
            | otherwise = bd' {square = next}
            where
                bd' = bd {ccCard = (ccCard bd + 1)`mod`16}
        moveCH bd
            | chCard bd == 0 = bd' {square = 0}
            | chCard bd == 1 = bd' {square = 10}
            | chCard bd == 2 = bd' {square = 11}
            | chCard bd == 3 = bd' {square = 24}
            | chCard bd == 4 = bd' {square = 39}
            | chCard bd == 5 = bd' {square = 5}
            | chCard bd == 6 = moveR bd' {square = next}
            | chCard bd == 7 = moveR bd' {square = next}
            | chCard bd == 8 = moveU bd' {square = next}
            | chCard bd == 9 = bd' {square = (square bd - 3) `mod`40}
            | otherwise = bd' {square = next}
            where
                bd' = bd {chCard = (chCard bd + 1)`mod`16}
        moveR bd
            | square bd == 7 = bd {square = 15}
            | square bd == 22 = bd {square = 25}
            | otherwise = bd {square = 5} 
        moveU bd
            | square bd == 7 = bd {square = 12}
            | square bd == 22 = bd {square = 28}
            | otherwise = bd {square = 12} 
