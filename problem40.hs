
main :: IO ()
main = do
    print $ problem40

problem40 :: Integer
problem40 = toNum 0 * toNum 9 * toNum 99 * toNum 999 * toNum 9999 * toNum 99999 * toNum 999999

decimalText :: [Char]
decimalText = concat [show x | x <- [1..]]

toNum :: Int -> Integer
toNum d = read [decimalText !! d]
