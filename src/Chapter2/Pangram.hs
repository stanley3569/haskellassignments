--11 .Pangram: (a) write using only foldl', (b) write it using map and filter, (c) write it using any
module Chapter2.Pangram where
import Data.Char
import Data.List


 ---using only foldl' ---
checkPangram :: String -> Bool
checkPangram xs =foldl'(\ alpha xs -> delete (toLower xs) alpha) ['a'..'z'] xs =="" 



--using map and filter
checkPangram1 :: String -> Bool
checkPangram1 xs = (foldl' (\string1 alpha -> string1++(filter (==alpha) ['a'..'z'])) [] (lowerText xs) )  == ((filter (/=' ') (lowerText xs) ) )

lowerText :: String -> String
lowerText xs = (map toLower xs)


--using any
pangram2 :: String -> Bool
pangram2 xs =(not) (any (False==) (map (\ alpha ->(any (alpha ==) (lowerText xs))) ['a'..'z']) )



isPangram :: String -> Bool
isPangram xs = all (`elem` lowerText xs) ['a'..'z']




