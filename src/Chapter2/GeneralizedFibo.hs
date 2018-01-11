module Chapter2.GeneralizedFibo where

import Data.List

fiboList :: Int -> [Int]
fiboList n= listOfFibo ( foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   (fiboList++[x+y])  ) ) (0,1,[])  [1..n*(n+10)] )


listOfFibo :: (Int,Int,[Int]) -> [Int]
listOfFibo (x,y,z) = z


evenListFibo :: Int ->Int -> [Int] 
evenListFibo n y= take n (filter (\x -> x `mod` y == 0) (fiboList n) )