module Chapter1.SumSquareDifference where

sumOfSquare :: Int -> Int -> Int -> Int
sumOfSquare n x sum = if(n<=x)
                        then sumOfSquare(n+1)  x (sum+(n*n))
                    else sum

squareOfSum :: Int -> Int -> Int -> Int
squareOfSum n x sum = if(n<=x)
                        then squareOfSum(n+1)  x (sum+n)
                    else sum*sum


difference :: Int -> Int
difference x = squareOfSum 0 x 0 - sumOfSquare 0 x 0