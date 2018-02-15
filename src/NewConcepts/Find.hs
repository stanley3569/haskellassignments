module NewConcepts.Find where


find1 :: (a -> Bool) -> [a] -> Maybe a
find1 xs list1=
    case list1 of
        [] -> Nothing
        (x:_) -> if(xs x)
                    then Just x
                 else find1 xs (tail list1)




findIndex1 :: (a -> Bool) -> [a] -> Int -> Maybe Int
findIndex1 xs list1 count=
        case list1 of
            [] -> Nothing
            (x:_) -> if (xs x)
                        then Just count
                     else findIndex1 xs (tail list1) (count+1)


elemIndex1 :: Eq a => a -> [a] -> Int -> Maybe Int
elemIndex1 xs list1 count = 
    case list1 of
        [] -> Nothing
        (x:_) -> if(xs==x)
                    then Just count
                 else elemIndex1 xs (tail list1) (count+1)






