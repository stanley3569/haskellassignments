module Chapter1.EvenFibonacci where
import Data.List

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1)+fibo(n-2)


fiboList x =map fibo [1..x]


evenCheck :: Int -> Bool
evenCheck n = n `mod` 2 == 0

sumEvenNumbers ::[Int] -> Int
sumEvenNumbers x = sum ( takeWhile(<4000000) (filter evenCheck x) )



main = do
    print ( fiboList 35 )
    print(   sumEvenNumbers( fiboList 35 )  )

