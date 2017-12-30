 --9. Even Fibonacci numbers: Modify the solution already given above using filter
 module Chapter2.EvenFibo where 
 import Data.List 

 fibo :: Int -> Int
 fibo 0 = 0 
 fibo 1 = 1
 fibo n = fibo (n-1) + fibo (n-2) 


--sumOfEvenFibo :: Int -> Int
--sumOfEvenFibo n = foldl' (\ total x -> if( (x`mod` 2) ==0 ) 
--                                        then (total + x)
--                                       else total) 0 (map fibo[1..n]) 

--foldl' (\ (x, y, fiboList) i -> (y, x + y, fiboList ++ [x+y])) (0, 1, []) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]


 --sumOfEvenFibo ::Int -> (Int,Int,[Int])
 --sumOfEvenFibo n = foldl' (  \(x,y,fiboList) i -> ( y, x+y ,   ( filter even(fiboList++[x+y]) )   )   ) (0,1,[]) [1..n]

 sumOfEvenFibo :: Int -> (Int,Int,[Int],Int)
 sumOfEvenFibo n = foldl' (  \(x,y,fiboList,total) i -> ( y, x+y ,   (fiboList++[x+y]) , (sum(filter even (fiboList++[x+y]) ) )   ) ) (0,1,[],0)  [1..n]