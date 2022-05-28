import Data.Array

main :: IO ()
main = do
    print $ sum $ map getCountForChecout [2..99]

getCountForChecout :: Int -> Int
getCountForChecout n = loop dlist 0
    where
        dlist = takeWhile (<=n) doubleList
        loop [] cnt = cnt
        loop (d:ds) cnt = loop ds (cnt + loop' m' 0)
            where
                m = n - d
                m' = m `div` 2
                loop' (-1) cnt = cnt
                loop' count cnt = loop' (count-1) (cnt + s)
                    where
                        isSame = count == (m - count)
                        s1 = numList !! count
                        s2 = numList !! (m - count)
                        s = if isSame then case s1 of
                                            1 -> 1
                                            2 -> 3
                                            3 -> 6
                                            _ -> 0
                                      else s1*s2   

doubleList :: [Int]
doubleList = [2,4..40] ++ [50]

numList :: [Int]
numList = elems $ loop array ([1..20]++[2,4..40]++[3,6..60]++[25,50])
    where
        loop arr [] = arr//[(0,1)]
        loop arr (n:ns) = loop (arr//[(n,t+1)]) ns
            where
                t = arr ! n
        array = listArray (0, 99) $ replicate 100 0 