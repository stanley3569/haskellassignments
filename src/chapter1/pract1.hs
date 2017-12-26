sumOfList :: [Int] -> Int -> Int
sumOfList arr total =
    if (length arr) == 0
        then total
    else (sumOfList(tail arr) (total + (head arr)))




evenList :: Int -> [Int] -> [Int]
evenList n lst = if(n==0)
                    then lst
                else if (n `mod` 2) == 0
                    then evenList (n-1) (n:lst)
                else evenList (n-1) lst





main = do
        print ( evenList 20 [] )
