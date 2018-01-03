module Chapter2.AddLineNumber where

listString n= map (\i -> (show i)++":") [1..n]


prefixLineNumber1 :: [String] -> [String]
prefixLineNumber1 xs = zipWith (++) (listString (length xs)) xs





--prefixLineNumber :: [String] -> [String] -> [String]
--prefixLineNumber xs string1= if(length xs<=0)                                then string1                            else prefixLineNumber (init xs) ( ((show(length xs) ) ++ tail(xs)++string1 ) )
