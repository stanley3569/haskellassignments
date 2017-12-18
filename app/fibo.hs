fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1)+fibo(n-2)


fiboList x =map fibo [1..x]


--evenSum :: Int -> Int -> Int
--evenSum n total = 



  --  evenList :: Int -> [Int] -> [Int]
  --  evenList n lst = if(n==0)
   --                     then lst
  --                  else if (n `mod` 2) == 0
  --                      then evenList (n-1) (n:lst)
   --                 else evenList (n-1) lst


--evenFibo :: Int -> Int -> Int
--evenFibo n total =


evenCheck :: Int -> Bool
evenCheck n = n `mod` 2 == 0

sumEvenNumbers ::[Int] -> Int
sumEvenNumbers x = sum (filter evenCheck x)



main = do
    print ( fiboList 20 )
    print(   sumEvenNumbers( fiboList 20 )  )

    --print(evenSum 10)