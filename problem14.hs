import Distribution.Simple.Utils (xargs)

main :: IO ()
main = do
    print $ problem14 [seqNums x | x <- [1..1*10^6-1]]

seqList:: Integer -> [Integer] -> [Integer]
seqList 1 xs = 1:xs
seqList n xs =
        let t = if n `mod` 2 == 0 then div n 2 else 3*n+1
        in seqList t (n:xs)

seqNums :: Integer -> (Integer, Int)
seqNums n = (n, length $ seqList n [])

problem14 :: [(Integer, Int)] -> Integer
problem14 xs = fst $ loop (1,0) xs
    where
        loop :: (Integer, Int) -> [(Integer, Int)] -> (Integer,Int)
        loop mx [] = mx
        loop mx (x:xs) = 
            let t = if snd mx < snd x then x else mx
            in loop t xs
