
main :: IO ()
main = do
    print $ problem15 20

problem15 :: Integer -> Integer
problem15 n = div (product [(n+1)..2*n]) (product [1..n])

