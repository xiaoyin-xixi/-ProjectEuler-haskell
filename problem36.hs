import Numeric

main :: IO ()
main = do
    print $ sum problem36

problem36 :: [Integer]
problem36 = [x | x <- [1..(10^6-1)], isValid x]
    where
        isValid :: Integer -> Bool
        isValid n = isPalindrome (show n) && isPalindrome (showBin n "")
        isPalindrome text = text == reverse text

