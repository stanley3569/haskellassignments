module Chapter3.RunLengthEncoding2 where
    
import Data.List

data Encoding = Multiple Int Char | Single Char deriving (Eq,Show,Ord)



encode :: String -> [Encoding]
encode inputString =encoding (group inputString) []


encoding :: [String] -> [Encoding] -> [Encoding]
encoding groupString xs=
    if (length groupString == 0)
        then xs
    else
        case (head groupString) of
                ([c]) ->encoding (tail groupString)  (xs ++ [Single c])
                (x) -> encoding (tail groupString)  (xs ++ [Multiple (length x) (head x)])

decode :: [Encoding] -> String
decode listString = decoding listString []              

decoding :: [Encoding] -> String -> String
decoding inputString xs=
    if (length inputString == 0)
        then xs
    else
        case (head inputString) of
            (Single x) ->decoding (tail inputString) (xs++ [x])
            (Multiple num x) ->decoding (tail inputString) (xs ++ (replicate num x) )






