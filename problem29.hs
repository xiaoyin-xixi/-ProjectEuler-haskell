import Data.List

main :: IO ()
main = do
    print $ length $ nub [a^b | a <- [2..100], b <- [2..100]]

