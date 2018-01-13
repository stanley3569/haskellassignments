module Chapter2.GeneralizedFibo1 where

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








                           

--fiboList :: Int -> [Int]
--fiboList n=  (listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] ) )


--listOfFibo :: (Int,Int,[Int]) -> [Int]
--listOfFibo (x,y,z) = z




{-}
listOfFibo :: (Int,Int,[Int]) -> [Int]
listOfFibo (x,y,z) = z



--fiboList :: Int -> [Int]
fiboList n= (     listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] )    )




--multipleOfThree :: Int -> [Int]
--multipleOfThree n= (take n( filter (\x -> x `mod` 3 == 0)  [1..] )  )
multipleOfThree n=  ( filter (\x -> x `mod` 3 == 0) ) n

         

--multipleOfFive ::Int -> [Int]
--multipleOfFive n= (take n( filter (\x -> x `mod` 5 == 0)  [1..] )  )
multipleOfFive n= ( filter (\x -> x `mod` 5 == 0)   (fiboList n) )



-}


{-}

listOfFibo :: (Int,Int,[Int]) -> [Int]
listOfFibo (x,y,z) = z


--fiboList :: Int -> [Int]
--fiboList n= take n( multipleOfThree1(  listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] )    ) )




fiboList n= ( (  listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n] )    ) )





multipleOfThree1 n =take (n) ( filter(\x -> x `mod` 3 == 0) (fiboList n) )



multipleOfFive1 x = filter(\x -> x `mod` 5 == 0) x




-}





{-}
--evensum :: Int -> Int -> Int -> Int -> Int
evensum maxValue a b total = if(b < maxValue)
                                then
                                    if (b `mod` 2) == 0
                                        then evensum maxValue b (a+b) (total+b)
                                    else evensum maxValue b (a+b) total
                              else total

-}


--module Chapter2.GeneralizedFibo where

--import Data.List


{-}
genFibo n a b emptylist = if(n<=0)
                            then emptylist
                          else checkeven n b (a+b) emptylist 
 -}



{-}

evenListFibo :: Int -> [Int] 
evenListFibo n = take n (filter (\x -> x `mod` 2 == 0) (fiboList n) )
-}


