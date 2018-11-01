module Chapter3.CalculateEndTime1 where
import Data.Char

data TimeOfDay = MkTimeOfDay Int Int deriving (Eq, Show, Ord)
data DurationInMins = MkDuration Int deriving (Eq, Show, Ord)

calculateEndTime :: TimeOfDay -> DurationInMins -> TimeOfDay
calculateEndTime (MkTimeOfDay hr mn) (MkDuration d) = 
    let (addHour, finalMins) = divMod (mn+d) 60
    in (MkTimeOfDay (hr + addHour) finalMins)




toUppercase :: String -> String
toUppercase str = map (\str -> toUpper str) str