--5. Generating a list of all even numbers till “N”: (a) Use foldl' alone, (b) Use filter and the short-hand of generating a list containing all number within a range:[1..n]
module Chapter2.EvenList where
import Data.List


evenList :: Int -> [Int]
evenList n = foldl' (\ evenNumber n -> if(n `mod` 2 == 0) 
                                        then (evenNumber ++ [n])
                                       else evenNumber) [] [1..n]


   
evenList1 :: Int -> [Int]
--evenList1 n = foldl' (\ evenNumber n -> filter even(evenNumber++[n]) ) [] [1..n]
evenList1 n =  filter even ( foldl' (\ evenNumber n ->(evenNumber++[n]) ) [] [1..n] )                                      
                                        
                                       