module Chapter3.DateArithmetic3 where
    
import Data.List



--data Day = MkDay Int deriving (Eq, Show, Ord)
--data Month = MkMonth Int deriving (Eq, Show, Ord)
--data Year = MkYear Int deriving (Eq, Show, Ord)

data Date = MkDate Int Int Int deriving (Eq, Show, Ord)

addDays :: Date -> Int -> Date
addDays (MkDate dd mm yy) daystoadd =
    let yearType = isLeapYear (yy)
      --  yearDays = yearLength (yy)
      --  yearTypeMonthList = monthLength (mm) (yy)
      --  daysRemainingInMonth = leftInMonth (dd,mm,yy)
      --  daysSinceStartYear = daysSinceYearBegan (dd, mm,yy)
     --   daysLeftInYear = leftInYear (dd,mm, yy)
        newDate = addDaysToDate (MkDate dd mm yy) (daystoadd)
    in newDate







isLeapYear :: Int -> Bool
isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


     
--yearLength :: Year -> Day                                        --days
--yearLength n = if (isLeapYear n) then ( MkDay(366) ) else ( MkDay(365) )

yearLength :: Int -> Int                                       --days
yearLength n = if (isLeapYear n) then 366 else 365



monthLength :: Int -> Int -> Int                                --days in a month of a particular year
monthLength (mm) (yy) = months !! (mm-1)  where
    months = if isLeapYear (yy) then months2 else months1
    months1 = [31,28,31,30,31,30,31,31,30,31,30,31]
    months2 = [31,29,31,30,31,30,31,31,30,31,30,31]


leftInMonth :: (Int,Int,Int) -> Int                                --no of days left in a month from current day
leftInMonth (dd,mm,yy) =   (monthLength mm yy) - dd      + 1         


daysSinceYearBegan :: (Int,Int,Int) -> Int                          --total days became since start of year
daysSinceYearBegan (dd,mm,yy) = (sum ( (take (mm-1) ) [monthLength (mm) yy] ) ) + dd



leftInYear :: (Int,Int,Int) -> Int                                      --total days left in year
leftInYear (dd,mm,yy) = yearLength (yy ) - (daysSinceYearBegan (dd,mm,yy) )


addDaysToDate :: (Date) -> Int -> (Date)
--addDaysToDate (dd,mm,yy) (0) = (dd,mm,yy)

addDaysToDate (MkDate dd mm yy) (days) = 
                if (days == 0) 
                    then (MkDate dd mm yy)
                else if ( (dd==(1) )&& (mm == (1) ) && ( days>=  (yearLength  yy ) ) ) 
                    then addDays (MkDate 1 1 (yy+1)) ( ( days) -  (yearLength yy )  )                           
                else  
                    if (  days >= (leftInYear (dd,mm,yy)) ) 
                        then addDays (MkDate dd mm (yy+1)) (days- (leftInYear (dd,mm,yy)) )
                    else if ( days >=  (leftInMonth (dd,mm,yy)  ) )
                        then addDays (if mm == (12) then (MkDate 1 1 (yy+1) ) else (MkDate 1 (mm+ 1) yy) ) (days- ( (leftInMonth (dd,mm,yy) ) ) )
                    else (  MkDate (dd + days ) mm yy)


