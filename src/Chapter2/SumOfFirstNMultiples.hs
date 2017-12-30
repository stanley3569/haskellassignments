module Chapter2.SumOfFirstNMultiple where

import Data.List





sumOfFirstNMultiple :: Int -> Int
sumOfFirstNMultiple n = foldl' (\total x -> if ((x `mod` 3) == 0) || ((x `mod` 5) == 0) 
                                                      then (total + x) 
                                               else total) 0 [1..n]





