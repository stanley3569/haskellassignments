import Data.Char


removeHypen :: String -> String
removeHypen x = isValidIsbn (filter (/='-') x)

isValidIsbn :: String -> String
isValidIsbn x = if( length x == 10 ) 
                      then x 
               else ""



changeToInt :: String -> [Int]
changeToInt xs =    ( (map digitToInt xs) )
--zipWith(*) [1..10]

calculateIsbn :: [Int] -> [Int]
calculateIsbn xs = (zipWith(*) [10,9..1] xs) 

checkIsbn :: [Int] -> Bool
checkIsbn xs = ( (sum (calculateIsbn xs)) `mod` 11 == 0 ) 
--checkIsbn xs = ( (sum (changeToInt xs)) `mod` 11 == 0 )         --(zipWith(*) [10,9..1] xs) 



main = do
    print   (checkIsbn (changeToInt(removeHypen "3-598-21508-8")) )                 --true
    print   (checkIsbn (changeToInt(removeHypen "3-598-21508-9")) )             --false


    --print   (removeHypen "3-423423-42822-22")