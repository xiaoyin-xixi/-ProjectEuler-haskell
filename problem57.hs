
main :: IO ()
main = do
    print $ foldl (\acc (a,b) -> acc + if (length $ show a) > (length $ show b) then 1 else 0) 0 $ take 1000 $ makeSequence (3,2)

makeSequence :: (Integer, Integer) -> [(Integer, Integer)]
makeSequence pair = (a,b) : makeSequence (a,b)
    where
        b = fst pair + snd pair
        a = snd pair + b
