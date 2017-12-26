module Chapter1.EvenFibonacci where
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo (n-2)


fiboList x =(map fibo [1..x])


sumOfFibo :: [Int] -> Int
sumOfFibo [x] =  if(x `mod` 2 == 0)
                    then x
                else 0

sumOfFibo x = if ( (head x) `mod`2 ==0)
                  then (head x) + sumOfFibo(tail x)
              else sumOfFibo(tail x)


main = do
        print (fiboList 10)
        print ( sumOfFibo (fiboList 10 ) )
        print (fiboList 20)
        print ( sumOfFibo (fiboList 20 ) )
