--11 .Pangram: (a) write using only foldl', (b) write it using map and filter, (c) write it using any

import Data.Char
import Data.List


 ---using only foldl' ---
checkPangram :: String -> Bool
checkPangram xs =foldl'(\ alpha xs -> delete (toLower xs) alpha) ['a'..'z'] xs =="" 



--using map and filter
--checkPangram1 :: String -> Bool
--checkPangram1 xs = foldl' (\string1 alpha -> string1++(filter alpha ['a'..'z'])) []
