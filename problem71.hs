import Data.Ratio
import Data.List

main :: IO ()
main = do
    print $ problem71

problem71 :: Integer
problem71 = numerator $ list !! (idx-1)
    where 
        (Just idx) = elemIndex (3%7) list
        list = sort [n%d | d <- [2..1000000], n <- [(4285*d`div`10000)..(3*d`div`7)], gcd n d == 1]
