module Chapter1.ListOfEven where
evenList :: Int -> [Int] -> [Int]
evenList n lst = if (n==0)
                    then lst
                 else if(n `mod` 2 ==0) 
                    then evenList(n-1) (n:lst)
                 else evenList(n-1) lst
 