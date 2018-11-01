module Chapter2.CalculateEndTime where

calculateEndTime :: Int -> Int -> Int -> (Int, Int)
calculateEndTime hr mn durationInMins =
  let (addHour, finalMins) = divMod (mn + durationInMins) 60
  in (hr + addHour, finalMins)


main = do
  print (calculateEndTime 01 15 45)