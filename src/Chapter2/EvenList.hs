 --6. Generate a list of first “N” even numbers: (a) Use foldl' alone, (b) Use map and [1..x] short-hand syntax
module Chapter2.EvenList where
import Data.List

evenList :: Int -> [Int]
evenList n = foldl' (\ evenNumber n -> evenNumber ++ [n*2] ) [] [1..n]
 

evenList1 :: Int -> [Int]
evenList1 n = map (\i -> i * 2) [1..n]


