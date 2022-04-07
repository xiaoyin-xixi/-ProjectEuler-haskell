import Data.List
import Control.Monad

main :: IO ()
main = do
    list <- readHandList
    print $ length $ filter (==True) $ problem54 $ handList list


readHandList :: IO [[String]]
readHandList = do
    hands <- readFile "p054_poker.txt"
    return $ map words $ lines hands

problem54 :: [(Hand, Hand)] -> [Bool]
problem54 list = loop [] list
    where
        loop win [] = win
        loop win (x:xs) = loop ((p1 > p2) : win) xs
            where
                (Just p1) = pokerHand $ fst x
                (Just p2) = pokerHand $ snd x

handList :: [[String]] -> [(Hand, Hand)]
handList l = loop [] l
    where
        loop hands [] = hands
        loop hands (x:xs) = loop ((sort $ toHand $ take 5 x, sort $ toHand $ drop 5 x) : hands) xs


data Suits = Heart | Club | Diamond | Spade deriving (Eq,Ord,Show)
data Card = Card { cardNum::Int, cardSuit::Suits} deriving (Eq,Ord)
type Hand = [Card] 

toHand :: [String] -> Hand
toHand [] = []
toHand (x:xs) = toCard x : toHand xs

toCard :: String -> Card
toCard (n:s:_)
    | s == 'H' = case n of
                    'T' -> Card 10 Heart
                    'J' -> Card 11 Heart
                    'Q' -> Card 12 Heart
                    'K' -> Card 13 Heart
                    'A' -> Card 14 Heart
                    _ -> Card (read [n]) Heart

    | s == 'C' = case n of
                    'T' -> Card 10 Club
                    'J' -> Card 11 Club
                    'Q' -> Card 12 Club
                    'K' -> Card 13 Club
                    'A' -> Card 14 Club
                    _ -> Card (read [n]) Club

    | s == 'D' = case n of
                    'T' -> Card 10 Diamond
                    'J' -> Card 11 Diamond
                    'Q' -> Card 12 Diamond
                    'K' -> Card 13 Diamond
                    'A' -> Card 14 Diamond
                    _ -> Card (read [n]) Diamond

    | otherwise = case n of
                    'T' -> Card 10 Spade
                    'J' -> Card 11 Spade
                    'Q' -> Card 12 Spade
                    'K' -> Card 13 Spade
                    'A' -> Card 14 Spade
                    _ -> Card (read [n]) Spade

pokerHand :: Hand -> Maybe Integer
pokerHand h = foldl mplus Nothing $ map (\f -> f h) f
    where
        f = [
            royalFlush,
            straightFlush,
            fourCard,
            fullHouse,
            flush,
            straight,
            threeCard,
            twoPair,
            onePair,
            highCard
            ]

royalFlush :: Hand -> Maybe Integer 
royalFlush h@(x:xs)
    | isStraight && isSameMark = Just basePoint
    | otherwise = Nothing
        where
            isStraight = (map (cardNum) h == [10..14])
            isSameMark = all (cardSuit x ==) $ map (cardSuit) xs
            basePoint = 2^143

straightFlush :: Hand -> Maybe Integer 
straightFlush h@(x:xs)
    | isStraight && isSameMark = Just point
    | otherwise = Nothing
        where
            isStraight = (map (cardNum) h == [cardNum x..cardNum x+4])
            isSameMark = all (cardSuit x ==) $ map (cardSuit) xs
            basePoint = 2^130
            point = 2^(cardNum x + 4 - 2) * basePoint

fourCard :: Hand -> Maybe Integer 
fourCard h@(x:xs)
    | isFour = Just point
    | otherwise = Nothing
        where
            numList = group $ map (cardNum) h
            pairList = filter (\x -> length x == 4) numList
            isFour = not $ null pairList
            basePoint = 2^117
            point = (foldl (\acc x -> acc + 2^(x-2)) 0 $ nub $ concat pairList)*basePoint

fullHouse :: Hand -> Maybe Integer 
fullHouse h@(x:xs)
    | isThree && isTwo = Just point
    | otherwise = Nothing
        where
            numList = group $ map (cardNum) h
            pairList = filter (\x -> length x == 3) numList
            isThree = not $ null pairList
            isTwo = not $ null $ filter (\x -> length x == 2) numList
            basePoint = 2^104
            point = (foldl (\acc x -> acc + 2^(x-2)) 0 $ nub $ concat pairList)*basePoint

flush :: Hand -> Maybe Integer 
flush h@(x:xs)
    | isSameMark = Just point
    | otherwise = Nothing
        where
            isSameMark = all (cardSuit x ==) $ map (cardSuit) xs
            basePoint = 2^91
            point = (foldl (\acc x -> acc + 2^(cardNum x-2)) 0 h) * basePoint

straight :: Hand -> Maybe Integer 
straight h@(x:xs)
    | isStraight = Just point
    | otherwise = Nothing
        where
            isStraight = (map (cardNum) h == [cardNum x..cardNum x+4])
            basePoint = 2^78
            point = 2^(cardNum x + 4 - 2) * basePoint

threeCard :: Hand -> Maybe Integer 
threeCard h@(x:xs)
    | isThree = Just point
    | otherwise = Nothing
        where
            numList = group $ map (cardNum) h
            pairList = filter (\x -> length x == 3) numList
            isThree = not $ null pairList
            basePoint = 2^65
            point = (foldl (\acc x -> acc + 2^(x-2)) 0 $ nub $ concat pairList)*basePoint

twoPair :: Hand -> Maybe Integer 
twoPair h@(x:xs)
    | isDouble = Just point
    | otherwise = Nothing
        where
            numList = group $ map (fromIntegral . cardNum) h
            pairList = filter (\x -> length x == 2) numList
            isDouble = (length pairList) == 2
            basePoint = 2^39
            a = foldl (\acc x -> acc + 2^(x-2)) 0 $ nub $ concat pairList
            point = (a*2^13 +  (foldl (\acc x -> acc + 2^(x-2)) 0 $ concat $ filter (\x -> length x == 1) numList)) * basePoint


onePair :: Hand -> Maybe Integer 
onePair h@(x:xs)
    | isOne = Just point
    | otherwise = Nothing
        where
            numList = group $ map (fromIntegral . cardNum) h
            pairList = filter (\x -> length x == 2) numList
            isOne = (length pairList) == 1
            basePoint = 2^13
            a = (head $ head pairList) - 2
            point = (a*2^13 +  (foldl (\acc x -> acc + 2^(x-2)) 0 $ concat $ filter (\x -> length x == 1) numList)) * basePoint

highCard :: Hand -> Maybe Integer 
highCard h@(x:xs)
    | isNoStraight && isNoSameMark && isNoPair = Just point
    | otherwise = Nothing
        where
            isNoStraight = not (map (cardNum) h == [cardNum x..cardNum x+4])
            isNoSameMark = not $ all (cardSuit x ==) $ map (cardSuit) xs
            numList = group $ map (cardNum) h
            isNoPair = (length $ filter (\x -> length x == 1) numList) == 5
            basePoint = 1
            point = (foldl (\acc x -> acc + 2^(cardNum x-2)) 0 h) * basePoint
