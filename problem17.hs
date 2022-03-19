
main :: IO ()
main = do
    print $ problem17

to10Texts = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
to20Texts = ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
to100Texts = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

to20List = tail to10Texts ++ ["ten"] ++ to20Texts
to100List = to20List ++ [x ++ y | x <- to100Texts, y <- to10Texts]
to1000List = to100List ++ [x ++ "hundredand" ++ y | x <- tail to10Texts, y <- to100List] ++ [x ++ "hundred" | x <- tail to10Texts] ++ ["onethousand"]

problem17 = length $ concat to1000List
