module Chapter2.PresentDays where
import Data.List

daysOfActualWork :: [Bool] -> [Bool] -> [Bool]
daysOfActualWork officeOpenDays secretaryPresentDays =
    zipWith (\isOfficeOpen isSecretaryPresent -> isOfficeOpen && isSecretaryPresent) officeOpenDays secretaryPresentDays


main = do
    print( daysOfActualWork [True,False,False,True] [False,True,False,True]    )