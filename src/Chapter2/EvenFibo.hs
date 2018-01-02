 --9. Even Fibonacci numbers: Modify the solution already given above using filter
 module Chapter2.EvenFibo where 
 import Data.List 


 sumOfEvenFibo :: Int -> (Int,Int,[Int],Int)
 sumOfEvenFibo n = foldl' (  \(x,y,fiboList,total) i -> ( y, x+y ,   (fiboList++[x+y]) , (sum(filter even (fiboList++[x+y]) ) )   ) ) (0,1,[],0)  [1..n]