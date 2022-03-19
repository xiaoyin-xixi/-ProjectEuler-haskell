
main :: IO ()
main = do
    print $ problem19

data Week = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show)
pairList :: [(Int, Int)]
pairList = [(1,31), (2,28), (3,31), (4,30), (5,31), (6,30), (7,31), (8,31), (9,30), (10,31), (11,30), (12,31)]

makeCalendar :: Int -> (Int, Int) -> [(Int, Int, Int)]
makeCalendar y (m, d) = loop (d+1) []
    where
        loop 0 _ = []
        loop n xs = xs ++ loop (n-1) [(y, m, (d-n+2))]

century20Calendar :: [(Int, Int, Int)]
century20Calendar = concat [ (loop y pairList) | y <- [1900..2000]]
    where
        loop :: Int -> [(Int, Int)] -> [(Int, Int, Int)]
        loop _ [] = []
        loop y ((m, d):xs)
            | m == 2 && isLeapYear = makeCalendar y (m, d+1) ++ (loop y xs)
            | otherwise = makeCalendar y (m, d) ++ (loop y xs)
            where 
                isLeapYear = if y `mod` 400 /= 0 && y `mod` 100 == 0 then False else if y `mod` 4 == 0 then True else False 

problem19 = length $ filter (\((y, m, d), w) -> y > 1900 && d == 1 && w == Sunday) $ zip century20Calendar $ cycle [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday] 
