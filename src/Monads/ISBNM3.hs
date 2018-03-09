module Monads.ISBNM3 where
    
import Data.Char
import Data.List 
import Data.List.Split

removeHypen :: String -> String
removeHypen x =(isValidIsbn (filter (/='-') x))


isValidIsbn :: String -> String
isValidIsbn x = if( length x == 10 ) 
                      then checkX (x)
                else "1234567890"

checkX :: String -> String
checkX xs =     if( (last(xs) )=='x')
                    then init(xs)               
                else if ( (last(xs))>='a' || (last(xs))>='A')
                    then "1234567890"
                else xs


changeToInt :: String -> [Int]
changeToInt xs =    if (length xs == 10)
                        then ( (map digitToInt xs) )
                    else ( (map digitToInt xs) ++ [10] )
                                                
calculateIsbn :: [Int] -> [Int]
calculateIsbn xs = (zipWith(*) [10,9..1] xs) 

checkIsbn :: [Int] -> Bool
checkIsbn xs = ( (sum (calculateIsbn xs)) `mod` 11 == 0 ) 
                                            
validateIsbn :: String -> Bool
validateIsbn xs = (checkIsbn (changeToInt(removeHypen xs)) )




checkIsbnList :: [String] -> [(String,Bool)]
checkIsbnList isbnL = map (\isbnN -> (isbnN,validateIsbn isbnN) ) isbnL


main1 :: IO ()
main1 = do
    putStrLn "enter the delimiter "
    delimiter <- getLine
    putStrLn "enter the data "
    instring <- getLine
    let spaceString = splitOn delimiter instring
    putStrLn (foldl' (\arr (x,y)-> arr ++pad 30 x ++ " | " ++ pad 20 (show y)  ++  "\n") [] (checkIsbnList  spaceString  ) )



pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')
