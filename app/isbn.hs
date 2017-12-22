import Data.Char

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

main = do
  print (validateIsbn "3-598-21508-8")             --true
  print (validateIsbn "3662392003")                --true
  print (validateIsbn"3-598-21507-xx")             --false
  print (validateIsbn "3-598-21508-9")             --false
  print (validateIsbn "3-598-21508-111")           --false
  print (validateIsbn "3598215088")                --true
  print (validateIsbn "3598a15088")                --false
  print (validateIsbn "3-598-21507-10")            --false
  print (validateIsbn "3-598-21507-x")             --true
  print (validateIsbn "3-423423-42822-22")         --false 
  print (validateIsbn "3-598-21507-A")             --false
  print (validateIsbn "3-598-21507-S")             --false
  print (validateIsbn "123-45")                    --false
  print (validateIsbn "123-45-0000-A")             --false
  print (validateIsbn "123-45-0000-x")             --false
