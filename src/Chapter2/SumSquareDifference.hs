--8. Sum square difference: Use foldl'

module Chapter1.SumSquareDifference where

import Data.List
    

sumOfSquare :: Int -> Int -- -> Int -> Int
sumOfSquare n = foldl'(\total n -> total + n ) 0 (map(^2)[1..n])
 
squareOfSum :: Int -> Int
squareOfSum n = (foldl' (\total n -> total + n ) 0 [1..n]) ^2

difference :: Int -> Int
difference x = squareOfSum x - sumOfSquare x



