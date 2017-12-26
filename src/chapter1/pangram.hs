import Data.Char 
import Data.List 



--isPangram :: String -> Bool
--isPangram xs =all (`elem` xs) ['a'..'z']    --working




checkPangram :: String -> String -> String
checkPangram [xs] x = delete xs x
checkPangram xs x = (  delete (head xs) (checkPangram (tail xs) x)   )


isPangram :: String -> Bool
isPangram xs = if( (checkPangram xs ['a'..'z']) == "" )
                  then True
               else False



fixedText :: String -> String
fixedText xs = (map toLower xs)

validatePangram :: String -> Bool
validatePangram xs = isPangram(fixedText xs)


main = do
    print (validatePangram ("The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Lay Dog ---missing last character of alphabets")     )
    print (validatePangram ("The Quick dfdss %&*^#()Brown Fox_+Jumps Over 1 2 3 The Layz Dog --all alpphabets are present"))
    print (validatePangram ("BCDEFGHIJKLMNOPQURSTUVWXYZZZZZZAAAAA"))