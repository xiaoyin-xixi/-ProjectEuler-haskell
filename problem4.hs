
main :: IO ()
main = do
    print $ maximum $ problem4 3

isPalindrome :: Integer -> Bool
isPalindrome n =
    let text = show n
    in text == reverse text

problem4 :: Integer -> [Integer]
problem4 nth = [x*y | x <- [10^(nth-1)..10^nth-1], y <- [10^(nth-1)..x], isPalindrome $ x*y]
