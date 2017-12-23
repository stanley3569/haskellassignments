import Data.Char
import Data.List
 

main = do
    print  ( map (\x -> toUpper x) "abc" )
    print  ( map (\i -> i * 2) [1, 2, 3] )
    print  ( map (\c -> digitToInt c) "3345" )
    print  ( filter (\x -> (mod x 2) == 0) [1, 2, 3, 4, 5, 6] )
    print  ( filter (\x -> isAlpha x) "Hello world!" )
    print  ( filter (\x -> x /= "") ["hello", "", "world"] )
    print  ( foldl' (\result x -> if (x `elem` result) then result else (x:result)) [] [1, 2, 3, 3, 2, 3, 1] )
    print  ( foldl' (\result x -> result ++ [x * 2]) [] [1, 2, 3] )
    print  ( map (\x -> x * 2) [1, 2, 3] )
    print  ( divMod 10 2 )







