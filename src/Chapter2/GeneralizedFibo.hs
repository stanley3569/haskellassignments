module Chapter2.GeneralizedFibo where

import Data.List


                        
checkeven :: Int -> Int -> Int -> [Int] -> [Int]
checkeven n a b evenlist =if(n>0)
                                then
                                     if ( (b `mod` 2) == 0  ) 
                                        then checkeven (n-1) b (a+b) (evenlist++[b])
                                    else checkeven n b (a+b) evenlist
                           else evenlist

                        
              
checkmul3 :: Int -> Int -> Int -> [Int] -> [Int]
checkmul3 n a b mul3list =if(n>0)
                                then
                                     if ( (b `mod` 3) == 0  ) 
                                        then checkmul3 (n-1) b (a+b) (mul3list++[b])
                                    else checkmul3 n b (a+b) mul3list
                           else mul3list








{-}
--fiboList :: Int -> [Int]
--fiboList n= listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n*(n+10)] )


listOfFibo :: (Int,Int,[Int]) -> [Int]
listOfFibo (x,y,z) = z

--
--evenListFibo :: Int ->Int -> [Int] 
--evenListFibo n y= take n (filter (\x -> x `mod` y == 0) (fiboList n) )

--------------------------------------------------------

multipleOfThree :: Int -> [Int]
multipleOfThree n= (take n( filter (\x -> x `mod` 3 == 0)  [1..] )  )
 

         

multipleOfFive ::Int -> [Int]
multipleOfFive n= (take n( filter (\x -> x `mod` 5 == 0)  [1..] )  )



fiboList :: Int -> [Int]
fiboList n= listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] )



--fiboList3 :: Int -> [Int]
fiboList3 n= ( ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] ))



{-
evensum :: Int -> Int -> Int -> Int -> Int
evensum maxValue a b total = if(b < maxValue)
                                then
                                    if (b `mod` 2) == 0
                                        then evensum maxValue b (a+b) (total+b)
                                    else evensum maxValue b (a+b) total
                              else total

-}


-}
