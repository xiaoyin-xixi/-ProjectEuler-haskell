import Data.List
import Data.List.Split
import Data.Bits

main :: IO ()
main = do
    list <- readCipherList
    print $ problem59 list

problem59 :: [Int] -> Int
problem59 list = sum $ decryptCipher (fst key) list
    where
        Just key = searchPwd list

readCipherList :: IO [Int]
readCipherList = do
    cipher <- readFile "p059_cipher.txt"
    return $ map read $ splitOn "," cipher

decryptCipher :: String -> [Int] -> [Int]
decryptCipher s xs = zipWith (\c x -> x `xor` (fromEnum c)) (cycle s) xs
    where
        l = length xs

searchPwd :: [Int] -> Maybe ([Char], [Char])
searchPwd l = find (\s -> contains " this " $ snd s) list
    where
        list = [((c1:c2:c3:[]), map toEnum $ decryptCipher (c1:c2:c3:[]) l) | c1 <- ['a'..'z'], c2 <- ['a'..'z'], c3 <- ['a'..'z']]

contains :: String -> String -> Bool
contains xs ys =
    let m = length xs
    in any (\n -> xs == (take m $ drop n ys)) [0..length ys-m]
