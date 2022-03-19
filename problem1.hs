import qualified Data.Set as S

main :: IO ()
main = do
    print $ problem1 1000

problem1 :: Int -> Int 
problem1 n = let
    z3 = S.fromList [3,6..n-1]
    z5 = S.fromList [5,10..n-1]
    z = S.union z3 z5
    in sum z
