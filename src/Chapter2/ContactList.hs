module ContactList where

import Data.Char (toLower)
import Data.List (foldl')

--groupNamesByAlphabet :: [String] -> (Char, [String])
--groupNamesByAlphabet names =

groupNamesByAlphabet :: Char -> [String] -> (Char, [String])
groupNamesByAlphabet alpha names =
  foldl'

    -- the lambda
    (\ (alphabet, collectedNames) name ->
        if (length name > 0) && (toLower (head name) == (toLower alphabet))
        then (alphabet, collectedNames ++ [name])
        else (alphabet, collectedNames)
    )

    -- the initial value
    --('a', [])
    (alpha,[])

    -- the list
    names


groupNamesByAllAlphabets :: [String] -> [(Char, [String])]
groupNamesByAllAlphabets names = 
    filter
    
        -- the filter condition -- notice how it's destructuring the tuple...
        (\ (alpha, collectedNames) -> length (collectedNames) > 0)
        (map (\alphabet -> groupNamesByAlphabet alphabet names) "abcdefghijklmnopqrstuvwxyz")



main = do
    print(groupNamesByAllAlphabets ["rooney","baines","ashley","vidic","aguero","vardy","hazard"])