--7. Sum of first “N” multiples of 3 or 5: (a) Use foldl' 
module Chapter2.SumOfFirstNMultiples where
import Data.List
 
sumOfFirstNMultiple :: Int -> Int
sumOfFirstNMultiple n = foldl' (\total x-> (total + x) ) 0 (take n(filter  checkModAdd [1..n*5] )  )
 
checkModAdd :: Int -> Bool 
checkModAdd x = if(x `mod` 3 == 0 || x `mod` 5 == 0)
                 then True 
               else False 
        