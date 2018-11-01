module Chapter2.GeneralizedFibo2 where
    
import Data.List

--fiboList :: Int -> [Int]
--fiboList n= listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] )


--listOfFibo :: (Int,Int,[Int]) -> [Int]
--listOfFibo (x,y,z) = z



fiboList1 valid n a b listfibo = if(n>0)
                                    then
                                        if ( valid b ) 
                                            then fiboList1 valid (n-1) b (a+b) (listfibo++[b])
                                        else fiboList1 valid n b (a+b) listfibo
                                  else listfibo



multipleOfThree :: Int -> [Int]
multipleOfThree n= fiboList1  (\x -> x `mod` 3 == 0)  n 0 1 []  
 

         

multipleOfFive ::Int -> [Int]
multipleOfFive n= fiboList1  (\x -> x `mod` 5 == 0)  n 0 1 []  


--multof3 n =take n( filter(\x -> even x) (fiboList n) )









