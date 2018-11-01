module NewConcepts.FunctionCompositionTest where
    





--even :: Int -> Bool

--not :: Bool -> Bool


myOdd :: Int -> Bool
myOdd x = not (even x)

myOdd1 :: Int -> Bool
myOdd1 = not . even



----function currying
    

powerc :: Float -> Float -> Float
powerc n b =
    if n == 0 then 1.0 else b * powerc (n-1) b


--square = powerc 2
--square 10
        --100.0

