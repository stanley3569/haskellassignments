module Typeclasses.Test2 where 

    
data DayOfWeek =Mon | Tue | Weds | Thu | Fri | Sat | Sun 
-- day of week and numerical day of month
data Date = Date DayOfWeek Int
instance Eq DayOfWeek where
    (==) Mon Mon= True
    (==) Tue Tue= True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False
instance Eq Date where
    (==) (Date weekday dayOfMonth) 
                (Date weekday' dayOfMonth') = 
        weekday == weekday' && dayOfMonth == dayOfMonth'



------

data CountList = MkCount [Int]
instance Eq CountList where
    (==) (MkCount x) (MkCount y) = if (length x == length y)
                                        then True
                                    else False




----------------------------


data Suit = Club | Diamond | Heart | Spade deriving (Read, Show, Enum, Eq, Ord)

data CardValue = Two | Three | Four| Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Show, Enum, Eq, Ord)

data Card = Card {value :: CardValue, suit :: Suit} deriving (Read, Show, Eq)

instance Ord Card where
    compare c1 c2 = compare (value c1, suit c1) (value c2, suit c2)



-------------------------------------------------

data Color = Red | Orange | Yellow | Green | Blue | Purple | White | Black | Custom Int Int Int 

instance Eq Color where
    Red==Red = True
    Orange==Orange = True
    Yellow==Yellow = True
    Green == Green = True
    Blue == Blue = True
    Purple == Purple = True
    White == White = True
    Black == Black = True
    (Custom r g b) == (Custom r' g' b') =
        r==r' && g==g' && b==b'
    _==_ = False    



--show :: Show a => a -> String
--showsPrec :: Show a => Int -> a -> String -> String
--showList :: Show a => [a] -> String -> String


instance Show Color where
    show Red = "Red"
    show Orange = "Orange"
    show Yellow = "Yellow"
    show Green = "Green"
    show Blue = "Blue"
    show Purple = "Purple"
    show White = "White"
    show Black = "Black"
    show (Custom r g b) = 
        "Custom "++ show r ++ " " ++ show g ++ " " ++ show b













