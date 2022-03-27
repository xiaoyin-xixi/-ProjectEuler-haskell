import Data.List

main :: IO ()
main = do
    print $ problem33

problem33 = 
    let (a,b) = foldl (\(a,b) (a',b') -> (a*a', b*b')) (1,1) cancelFractionList
        m = gcd a b
    in b `div` m

cancelFractionList :: [(Int, Int)]
cancelFractionList = [(a,b) | b <- [11..99], a <- [11..(b-1)], isValid (a,b) $ getCancelFrac (a,b)]
        where
            isVaild _ Nothing = False
            isValid (a,b) (Just (a',b')) = (a `div` m, b `div` m) == (a' `div` m', b' `div` m')
                where
                    m = gcd a b
                    m' = gcd a' b'
            isValid _ _ = False

getCancelFrac :: (Int, Int) -> Maybe (Int, Int)
getCancelFrac (a, b)
    | mod a 10 == 0 = Nothing 
    | mod b 10 == 0 = Nothing 
    | null $ intersect a' b' = Nothing 
    | otherwise = if null (a'\\b') then Nothing else Just (read (a'\\b'), read (b'\\a'))
        where
            a' = show a
            b' = show b
