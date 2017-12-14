leapyear :: Int -> Bool
leapyear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


main = do
    print $ leapyear 2016
    print $ leapyear 1993