import Data.Char 
import Data.List 


isPangram :: String -> Bool
isPangram xs alpha= (delete ['a'..'b']  xs) 


fixedText :: String -> String
fixedText xs = map toLower xs




main = do
    
    print (isPangram (fixedText "The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Lay Dog ---missing last character of alphabet"))
    print (isPangram (fixedText "The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Layz Dog --all alpphabets are present") )