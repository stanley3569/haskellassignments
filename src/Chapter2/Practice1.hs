import Data.List

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  


numLongChains::Int
numLongChains = length ( filter(\xs -> length xs > 15) (map chain [1..100]))


sum':: (Num a)=> [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  



main = do 
        print (foldl' (\(x,y,fiboList) i->(y,x+y,fiboList++[x+y])) (0,1,[]) [1,2,3,4,5,6,7,8,9,10])
        print (deleteBy (\x y -> y `mod` x == 0) 4 [6,8,10,12])
        print (sortBy compare [3,2,7,3,13,64,54,5,2,1])


