--ISBN Verifier: (a) Build your solution using only foldl', (b) modify you solution to use zip or zipWith at the appropriate place.

module Chapter2.ISBN where
import Data.Char
import Data.List

isbn :: String -> Bool
isbn xs =  ( length(removeHypen(xs)) ==10)                  

removeHypen :: String -> String
removeHypen xs = (filter (/='-') xs )



checkIsbn :: String -> Bool
checkIsbn xs = (isbn xs) &&  ( (not) ('X' `elem` take 9 (removeHypen xs ) ) ) &&  all (\c -> c `elem` ['0','1','2','3','4','5','6','7','8','9','X']) (removeHypen xs)
                    
  
validIsbn :: String -> Bool
validIsbn xs = (checkIsbn xs) && ( sum( (zipWith(*) [10,9..1] (map (\c -> (digitToInt c)) (removeHypen xs)  ))) `mod` 11) == 0


--validateX :: String -> String
--validateX xs = (filter (/='X') (init (changeX xs) ))

--calculateIsbn :: [Int] -> [Int]
--calculateIsbn xs = (zipWith(*) [10,9..1] xs) 


 
--convertX :: String -> String
--convertX xs = 

--validIsbn :: String -> Bool
--validIsbn xs =isbn xs && (all (\c -> c `elem` ['0','1','2','3','4','5','6','7','8','9','X']) (removeHypen xs) ) 




