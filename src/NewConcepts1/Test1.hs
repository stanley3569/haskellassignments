module Typeclasses.Test1 where 

import Data.Char

data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

---------------------------------------------------

stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = toLower x == toLower y && stringEq xs ys
stringEq _ _ = False

------------------------------------------------------

class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False



-------------------------------------------------------
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"




------------------------------

data Mood = Blah
instance Show Mood where 
show1 _ = "Blah"


--------------------------------------------------------

data Digit = Zero | One | Two
--type Digits = [Digit]
data Sign = Pos | Neg
data Numeral = Num Sign Digit

instance Eq Sign where
  (==) Pos Pos = True
  (==) Neg Neg = True
  (==) _ _ = False

instance Eq Digit where
  (==) Zero Zero = True
  (==) One One = True
  (==) Two Two = True
  (==) _ _ = False

instance Eq Numeral where
    Num s1 x1 == Num s2 x2 = if(s1 == s2 && x1 == x2)
                                    then True
                                  else False


------------------------------------------



