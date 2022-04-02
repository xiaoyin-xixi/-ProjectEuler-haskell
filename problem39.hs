
-- 0<a<=b<=c
-- p=a+b+c
-- a+b>c
-- b-a<c
-- 0<a<=p/4     p/2-a<b<=(p-a)/2
-- p/4<a<=p/3   a<=b<=(p-a)/2

main :: IO ()
main = do
    print $ fst $ foldl (\(p,l) (p',l') -> if l < l' then (p',l') else (p,l)) (0,0) problem39

problem39 :: [(Integer, Int)]
problem39 = [(p, len p) | p <- [3..1000]]
    where 
        len p = length $ filter (\(a,b,c) -> a^2+b^2 == c^2) $ makeTriangleList p


makeTriangleList :: Integer -> [(Integer, Integer, Integer)]
makeTriangleList p = [(a,b,p-(a+b)) | a <- [1..(p `div` 4)], b <- [((p `div` 2)-a+1)..((p-a) `div` 2)]] ++
                     [(a,b,p-(a+b)) | a <- [((p `div` 4)+1)..(p `div` 3)], b <- [a..((p-a) `div` 2)]]

