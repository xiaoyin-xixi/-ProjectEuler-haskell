import Data.List

main :: IO ()
main = do
    print $ problem85

limit :: Int
limit = 100

problem85 :: Int
problem85 = if (p-2000000) < (2000000-p') then m*n else m'*n'
    where
        list = zip [0..] $ sortOn fst [(totalRectangle (m,n), (m,n)) | m <- [1..limit], n <- [1..m-1]]
        (idx,(p,(m,n))) = head $ dropWhile (\(_,(p,(_,_))) -> 2000000 > p) list
        (_,(p',(m',n'))) = list !! (idx - 1)

countRectangle :: (Int, Int) -> (Int, Int) -> Int
countRectangle (m,n) (m',n') = (m-m'+1) * (n-n'+1)

totalRectangle :: (Int, Int) -> Int
totalRectangle (m,n) = foldl (\acc r -> acc + countRectangle (m,n) r) 0 [(m',n') | m' <- [1..m], n' <- [1..n]]
