
module Chapter2.ContactList1 where
    
    import Data.Char
    import Data.List 
    

    
    
    groupAlpha :: [String] -> [[String]]
    groupAlpha names = groupBy (\x y-> (toLower(head x)==toLower(head y))) (sort (names) )
    
    groupNamesByAllAlphabets :: [String] -> [(Char, [String])]
    groupNamesByAllAlphabets names = foldl'(\contactList nameHead -> contactList++[(head (head nameHead), (nameHead) )]) [] (groupAlpha (names) )
        
    
    
        --filter
        
            -- the filter condition -- notice how it's destructuring the tuple...
           -- (\ (alpha, collectedNames) -> length (collectedNames) > 0)
           -- (map (\alphabet -> groupNamesByAlphabet alphabet names) "abcdefghijklmnopqrstuvwxyz")
    
    
    
    --main = do
     --   print(groupNamesByAllAlphabets ["rooney","baines","ashley","vidic","aguero","vardy","hazard","alexis"])