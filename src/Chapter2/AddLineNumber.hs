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








                          
--prefixLineNumber4 :: [String] -> String
--prefixLineNumber4 xs = zipWith3 (\x y z -> x++y++z) ["1"] [": "] ["main","print"]


--prefixLineNumber :: [String] -> [String] -> [String]
--prefixLineNumber xs string1= if(length xs<=0)                                then string1                            else prefixLineNumber (init xs) ( ((show(length xs) ) ++ tail(xs)++string1 ) )


--prefixLineNumber3 xs string1= if ((length xs)<=0)
--                            then string1

--prefixLineNumber3 :: [String] -> [String]
--prefixLineNumber3 xs = foldl'(\x y-> x++y) [] [xs]  [(map (\x -> (show x)++":" ) [1..(length xs)] )]





--main = do
    --print (prefixLineNumber2 ["main","print","helloworld","exit"])
   -- print (intersperse "hi " ["1:","2:"])
    --print (intercalate "hi " ["1:","2:"])
    --print( concat (intersperse "1" ["main","print"]))
    --print( zipWith3 (\x y z -> x++y++z) ["1"] [": "] ["main","print"] )



--prefixLineNumber3 :: [String] -> [String] --[[Char]] --String]]
--prefixLineNumber3 xs = foldl'(\x y-> x++y) [] [ (  ((map (\x -> (show x)++":") [1..(length xs)]),xs   ) ] 
