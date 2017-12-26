module Chapter1.SumOfFirstNMultiple where
sumOfNmultiples :: Int -> Int -> Int -> Int -> Int
sumOfNmultiples x sum i count= if (count<=x)
                              then 
                                if(x==0)
                                    then 0
                                else if(i `mod` 3 ==0 || i `mod` 5 ==0)
                                    then sumOfNmultiples x  (sum+i) (i+1) (count+1)                                   --( x + sumofmultiple(x-1) )
                                else sumOfNmultiples x sum (i+1) (count)
                             else sum
 




main = do
 print (sumOfNmultiples 10 0 0 0)
 print (sumOfNmultiples 20 0 0 0)
 print (sumOfNmultiples 20 0 0 0)