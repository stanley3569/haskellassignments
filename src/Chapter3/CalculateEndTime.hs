module Chapter3.CalculateEndTime where

{-}
calculateEndTime :: Int -> Int -> Int -> (Int, Int)
calculateEndTime hr mn durationInMins =
    let (addHour, finalMins) = divMod (mn + durationInMins) 60
    in (hr+addHour, finalMins)
-}

data TimeOfDay = MkTimeOfDay Int Int deriving (Eq,Show,Ord)

calculateEndTime :: TimeOfDay -> Int -> TimeOfDay
calculateEndTime (MkTimeOfDay hr mn) durationInMins = 
    let (addHour, finalMins) = divMod (mn + durationInMins) 60
        in MkTimeOfDay (hr + addHour) finalMins




calculateEndTime1 :: TimeOfDay -> Int -> TimeOfDay
calculateEndTime1 t durationInMins =
    let (MkTimeOfDay hr mn) = t
        (addHour, finalMins) = divMod (mn + durationInMins) 60
    in (MkTimeOfDay (hr + addHour) finalMins)

