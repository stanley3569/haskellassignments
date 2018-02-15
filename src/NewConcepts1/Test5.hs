module Typeclasses.Test5 where 


--data Maybe a = Nothing | Just a

maybeReverse :: Maybe [a] -> Maybe [a]
maybeReverse Nothing = Nothing
maybeReverse (Just xs) = Just $ reverse xs
