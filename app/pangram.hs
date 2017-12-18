import Data.Char 
import Data.List 


isPangram :: String -> String
isPangram xs = (delete ['a'..'z'] xs)

--isPangram :: String -> Bool
--isPangram xs =all (`elem` xs) ['a'..'z']    --working

fixedText :: String -> String
fixedText xs = map toLower xs


--'a','b','c'

main = do
    print (isPangram (fixedText "The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Lay Dog ---missing last character of alphabets"))
    print (isPangram (fixedText "The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Layz Dog --all alpphabets are present"))