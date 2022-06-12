
main :: IO ()
main = do
--    let d=80
--    print $ map (\s -> ((s,d),upN s d)) [0..9]
--    print $ map (\s -> ((s,d),downN s d)) [0..9]
    print $ problem113

problem113 :: Integer
problem113 = foldl (\acc n -> acc + ttlN n) 0 [1..100] 

ttlN :: Integer -> Integer
ttlN 1 = 9
ttlN digit = loop 1 0
    where
        loop 10 ttl = ttl
        loop s ttl = loop (s+1) (ttl + nonBouncyN s digit)

nonBouncyN :: Integer -> Integer -> Integer
nonBouncyN s d = upN s d + downN s d - 1

upN :: Integer -> Integer -> Integer
upN s 2 = 9 - s + 1
upN s 20 = let Just x = lookup (s,20) cacheUp20 in x
upN s 40 = let Just x = lookup (s,40) cacheUp40 in x
upN s 60 = let Just x = lookup (s,60) cacheUp60 in x
upN s 80 = let Just x = lookup (s,80) cacheUp80 in x
upN s d = loop s 0
    where
        loop 10 ttl = ttl
        loop cnt ttl = loop (cnt+1) (ttl + upN cnt (d-1))

downN :: Integer -> Integer -> Integer
downN s 2 = s + 1
downN s 20 = let Just x = lookup (s,20) cacheDown20 in x
downN s 40 = let Just x = lookup (s,40) cacheDown40 in x
downN s 60 = let Just x = lookup (s,60) cacheDown60 in x
downN s 80 = let Just x = lookup (s,80) cacheDown80 in x
downN s d = loop s 0
    where
        loop (-1) ttl = ttl
        loop cnt ttl = loop (cnt-1) (ttl + downN cnt (d-1))

cacheUp20 = [((0,20),6906900),((1,20),2220075),((2,20),657800),((3,20),177100),((4,20),42504),((5,20),8855),((6,20),1540),((7,20),210),((8,20),20),((9,20),1)]
cacheDown20 = [((0,20),1),((1,20),20),((2,20),210),((3,20),1540),((4,20),8855),((5,20),42504),((6,20),177100),((7,20),657800),((8,20),2220075),((9,20),6906900)]

cacheUp40 = [((0,40),1677106640),((1,40),314457495),((2,40),53524680),((3,40),8145060),((4,40),1086008),((5,40),123410),((6,40),11480),((7,40),820),((8,40),40),((9,40),1)]
cacheDown40 = [((0,40),1),((1,40),40),((2,40),820),((3,40),11480),((4,40),123410),((5,40),1086008),((6,40),8145060),((7,40),53524680),((8,40),314457495),((9,40),1677106640)]

cacheUp60 = [((0,60),49280065120),((1,60),6522361560),((2,60),778789440),((3,60),82598880),((4,60),7624512),((5,60),595665),((6,60),37820),((7,60),1830),((8,60),60),((9,60),1)]
cacheDown60 = [((0,60),1),((1,60),60),((2,60),1830),((3,60),37820),((4,60),595665),((5,60),7624512),((6,60),82598880),((7,60),778789440),((8,60),6522361560),((9,60),49280065120)]

cacheUp80 = [((0,80),571350360240),((1,80),58433559570),((2,80),5373200880),((3,80),437353560),((4,80),30872016),((5,80),1837620),((6,80),88560),((7,80),3240),((8,80),80),((9,80),1)]
cacheDown80 = [((0,80),1),((1,80),80),((2,80),3240),((3,80),88560),((4,80),1837620),((5,80),30872016),((6,80),437353560),((7,80),5373200880),((8,80),58433559570),((9,80),571350360240)]
