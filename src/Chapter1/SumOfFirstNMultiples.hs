module Chapter1.SumOfFirstNMultiple where
sumOfNmultiples :: Int -> Int -> Int -> Int
sumOfNmultiples x sum i = if (x>=0)
                              then 
                                 if(i `mod` 3 ==0 || i `mod` 5 ==0)
                                    then sumOfNmultiples (x-1)  (sum+i) (checkMod(i+1))                               --( x + sumofmultiple(x-1) )
                                 else 0 
                           else sum
 

checkMod :: Int -> Int
checkMod x = if(x `mod` 3 == 0 || x `mod` 5 == 0)
                then x
             else checkMod(x+1)



main = do
 print (sumOfNmultiples 10 0 0)          --119
 print (sumOfNmultiples 20 0 0)          --450
 print (sumOfNmultiples 30 0 0)          --998
 print (sumOfNmultiples 35 0 0)          --1350
 print (sumOfNmultiples 40 0 0)          --1758
 print (sumOfNmultiples 43 0 0)          --2028