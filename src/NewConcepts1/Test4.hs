module Typeclasses.Test4 where 
   


data TrafficLight = Red | Yellow | Green  


{-}
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  
-}

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  



-------------------
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((k,v):rest) =
    if key == k
       then Just v
    else myLookup key rest





