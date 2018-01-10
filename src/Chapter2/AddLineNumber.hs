module Chapter2.AddLineNumber where

import Data.List
import Data.Char

--zipWith
prefixLineNumber1 :: [String] -> [String]
--prefixLineNumber1 xs = zipWith (++) (map (\x -> (show x)++":") [1..(length xs)]) xs
prefixLineNumber1 xs = zipWith (\x y -> x++y) (map (\x -> (show x)++":") [1..(length xs)]) xs

--zip
prefixLineNumber2 :: [String] -> [String]
prefixLineNumber2 xs = foldl1' (\x y -> x++y) ( map (\(x,y) -> [x++y] ) ( zip (map (\x -> (show x)++":" ) [1..(length xs)]) xs ) )

--prefixLineNumber2 xs = foldl1 (\x y -> x++y) $ map (\(x,y) -> [x++y] ) ( zip (map (\x -> (show x)++":" ) [1..(length xs)]) xs )


--prefixLineNumber3 :: [String] -> [[String]]
--prefixLineNumber3 xs = scanl1 (\x y -> x++y) ( map (\(x,y) -> [x++y] ) ( zip (map (\x -> (show x)++":" ) [1..(length xs)]) xs ) )
 


--recurssive
prefixLineNumber4 :: [String] -> [String]
prefixLineNumber4 xs =prefixNumberToLine (map (\x -> (show x)++":") [1..(length xs)] )  xs


prefixNumberToLine :: [String] -> [String] -> [String] 
prefixNumberToLine numList xs = if(xs == [])
                            then numList
                          else [(head numList ++ head xs)] ++ prefixNumberToLine (tail numList) (tail xs)



