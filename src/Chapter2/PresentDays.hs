module Chapter2.PresentDays where
import Data.List

daysOfActualWork :: [Bool] -> [Bool] -> (Int, [Bool])
daysOfActualWork officeOpenDays secretaryPresentDays =
    foldl'
     (\(i, finalList) isOfficeOpen ->
       (i + 1, finalList ++ [isOfficeOpen && (secretaryPresentDays !! i)]))
     (0, [])
     officeOpenDays


main = do
    print(daysOfActualWork [True,False,False,True] [False,True,False,True]  )