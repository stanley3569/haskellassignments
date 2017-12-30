--Double a list of integers using map
module Chapter2.DoubleList where
import Data.List

doubleList :: [Int] -> [Int]
doubleList lst = map(\i ->  i * 2) lst


main = do
    print(doubleList [1..10])



