import Data.Array

main :: IO ()
main = do
    print $ problem72 - 1

problem72 :: Integer
problem72 = sum $ eulerFuncSequence 1000000

eulerFuncSequence :: Integer -> [Integer]
eulerFuncSequence n = elems $ loop [2..n] phiArray
    where
        loop [] phi = phi
        loop (x:xs) phi
            | phi ! x == x = loop xs (phi // loop' [x,2*x..n])
            | otherwise = loop xs phi
                where
                    loop' [] = []
                    loop' (y:ys) = s `seq` (y,s) : loop' ys
                        where
                            s = (phi ! y) * (x-1) `div` x
        phiArray = listArray (1,n) [1..n]
