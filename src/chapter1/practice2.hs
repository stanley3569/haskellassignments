import Data.Char (toUpper)

sumOfList :: Int -> [Int] -> Int
sumOfList total x =   if(x== [])
                        then total
                      else sumOfList(total + (head x)) (tail x)




---------------------------------------

sumOfEven :: Int -> [Int] -> Int
sumOfEven total x = if( x == [])
                        then total
                    else if( head(x) `mod` 2 ==0 )
                        then sumOfEven (total + (head x)) (tail x)
                    else sumOfEven total (tail x)


--------------------------------------

doubleList :: [Int] -> [Int] -> [Int]
doubleList processedList remainingList = if(remainingList == [])
                                            then processedList
                                        else doubleList(processedList ++ [(head remainingList) * 2]) (tail remainingList)


--------


toUppercase :: [Char] -> [Char] -> [Char]
toUppercase processString remainingstring = if(remainingstring == [])
                                                then processString
                                            else toUppercase (processString ++ [toUpper (head remainingstring)]) (tail remainingstring)



-------------

isCharPresent :: Char -> [Char] -> Bool
isCharPresent needle remainingString = if(remainingString == [])
                                            then False
                                        else if(needle == (head remainingString ))
                                            then True
                                        else isCharPresent needle (tail remainingString)

allCharsPresent :: [Char] -> String -> Bool
allCharsPresent remainingNeedles haystack = if(remainingNeedles == [])
                                                then True
                                            else if isCharPresent(head remainingNeedles) haystack
                                                then allCharsPresent(tail remainingNeedles) haystack
                                            else False


-----------------------

evenList :: Int -> [Int] -> [Int]
evenList n lst = if (n==0)
                    then lst
                 else if(n `mod` 2 ==0) 
                    then evenList(n-1) (n:lst)
                 else evenList(n-1) lst





-------


sumOfListInternal :: Int -> [Int] -> Int
sumOfListInternal total x = if(x==[])
                                then total
                            else sumOfListInternal (total + (head x)) (tail x)


sumOfList1 :: [Int] -> Int
sumOfList1 x = sumOfListInternal 0 x


-----------------------------------------------------

factorial n = if (n<2)
                then 1
              else n * factorial (n-1)


------------------------
