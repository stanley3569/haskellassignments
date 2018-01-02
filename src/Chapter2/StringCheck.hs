 --4. Check if all characters in a String are contained within another String: Use foldl' & elem

 module Chapter2.StringCheck where

 import Data.List

 stringCheckElem :: String -> Char -> Bool
 stringCheckElem string stringAlpha = stringAlpha `elem` string

 stringCheck :: String -> String -> Bool
 stringCheck string1 string2 = if( foldl'(\stringNew stringChar -> if(stringCheckElem string2 stringChar)
                                                                    then stringNew++[stringChar]
                                                              else stringNew)  [] string1 ) == string1
                                  then True
                                else False 


