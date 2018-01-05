--11 .Pangram: (a) write using only foldl', (b) write it using map and filter, (c) write it using any
module Chapter2.Pangram where
import Data.Char
import Data.List


 ---using only foldl' ---
checkPangram :: String -> Bool
checkPangram xs =foldl'(\ alpha xs -> delete (toLower xs) alpha) ['a'..'z'] xs =="" 



--using map and filter
checkPangram1 :: String -> Bool
checkPangram1 xs = (foldl' (\string1 alpha -> string1++(filter (alpha==) ['a'..'z'])) [] (map toLower xs) )  == ((filter (/=' ') (map toLower xs) ) )


isPangram :: String -> Bool
isPangram xs = all (`elem` map toLower xs) ['a'..'z']



pangram2 :: String -> Bool
pangram2 xs =(not) (any (False==) (map (\ alpha ->(any (alpha ==) (map toLower xs))) ['a'..'z']) )
--pangram2 xs = (  any (`elem` (map toLower xs \\ ['a'..'z']) ) ['a'..'z'] ) 
--pangram2 xs = (any (`notElem` ['a'..'z']) (map toLower xs) )


--main = do
--    print (pangram2 ("The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Lay Dog ---missing last character of alphabets")     )
--    print (pangram2 ("The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Layz Dog --all alpphabets are present"))
--    print (pangram2 ("BCDEFGHIJKLMNOPQURSTUVWXYZZZZZZAAAAA"))


--pangram3 :: String -> Bool
--pangram3 xs = any(\c -> checkPangram2 c) (map toLower xs)

--checkPangram2 :: Char -> Bool
--checkPangram2 c = (elem c ['a'..'z'])

--pangram3 :: String -> Bool
--pangram3 xs = (foldl'(\ alpha -> (any (alpha ==) (map toLower xs)) ) ['a'..'z'] ) 

--(map (\ alpha ->(any (alpha ==) (map toLower xs))) ['a'..'z'])