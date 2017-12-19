import Data.List

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1)+fibo(n-2)


fiboList x =map fibo [1..x]



  --  evenList :: Int -> [Int] -> [Int]
  --  evenList n lst = if(n==0)
   --                     then lst
  --                  else if (n `mod` 2) == 0
  --                      then evenList (n-1) (n:lst)
   --                 else evenList (n-1) lst




evenCheck :: Int -> Bool
evenCheck n = n `mod` 2 == 0

sumEvenNumbers ::[Int] -> Int
sumEvenNumbers x = sum ( takeWhile(<4000) (filter evenCheck x) )



main = do
    print ( fiboList 30 )
    print(   sumEvenNumbers( fiboList 30 )  )

