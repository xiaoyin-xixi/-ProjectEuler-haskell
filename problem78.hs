import Data.List
import Control.Monad
import Control.Monad.ST
import Control.Monad.Cont
import Data.Array
import Data.Array.ST

main :: IO ()
main = do
    print $ problem78

problem78 :: Maybe Int
problem78 = findIndex (\cnt -> cnt `mod` 1000000 == 0) $ elems $ countExpressions limit array
    where
        array = listArray (0,limit+1) $ 1:replicate (limit + 1) 0

withBreak :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> ST s r
withBreak = flip runContT pure . callCC

countExpressions :: Int -> Array Int Integer -> Array Int Integer
countExpressions n arr = runSTArray $ do
    stArray <- thaw arr
    forM_ [1..n+1] $ \m -> do
        withBreak $ \break ->
            forM_ [0..] $ \k -> do
                let mp = k * (3*k-1) `div` 2
                let mm = k * (3*k+1) `div` 2
                let sign = if k`mod`2 == 1 then 1 else -1
                when (m<mp) $ break ()
                addp <- lift $ readArray stArray (m-mp)
                org <- lift $ readArray stArray m
                lift $ writeArray stArray m (org + sign * addp)
                when (m<mm) $ break ()
                addm <- lift $ readArray stArray (m-mm)
                org <- lift $ readArray stArray m
                lift $ writeArray stArray m (org + sign * addm)
    return stArray

limit :: Int
limit = 100000
