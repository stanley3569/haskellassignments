module Chapter1.SumOfMultiple where
sumofmultiple x = if(x==0)
                    then 0
                else if(x `mod` 3 ==0 || x `mod` 5 ==0)
                    then ( x + sumofmultiple(x-1) )
                else sumofmultiple(x-1)





main = do
 print (sumofmultiple 999)