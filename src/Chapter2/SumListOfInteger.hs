 --1. Sum a list of integers using foldl'
 module Chapter2.SumListOfInteger where
 import Data.List

 sumListOfInteger :: Int->[Int]->Int
 sumListOfInteger total lst = 
                                 foldl' (\total lst -> total + lst) total lst


 main = do
        print(sumListOfInteger 0 [1..10])