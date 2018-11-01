module Chapter1.SumSquareDifference where

sumfun :: Int -> Int
sumfun n = (sum [1..n]^2) -  (sum (map(^2)[1..n]) )


main = do
    print (sumfun 10)
    