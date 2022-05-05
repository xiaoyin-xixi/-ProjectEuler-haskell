
main :: IO ()
main = do
    print $ length $ filter (uncurry checkRightTriangle) $ pointList [(x,y) | x <- [0..limit], y <- [0..limit], (x,y) /= (0,0)]

limit :: Int
limit = 50

checkRightTriangle :: (Int, Int) -> (Int, Int) -> Bool
checkRightTriangle (x,y) (x',y')
    | a > b = if c > a then c == a + b else a == b + c
    | c > b = c == a + b
    | otherwise = b == a + c
    where
        a = x^2 + y^2
        b = x'^2 + y'^2
        c = (x-x')^2 + (y-y')^2

pointList :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pointList [] = []
pointList (x:xs) = [(x,y) | y <- xs, isValid x y] ++ pointList xs
    where
        isCheck f p p' = f p == 0 && f p' == 0
        isValid p p' = not (isCheck fst p p' || isCheck snd p p')

