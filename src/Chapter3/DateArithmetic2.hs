module Chapter3.DateArithmetic2 where
    
import Data.List



data Day = MkDay Int deriving (Eq, Show, Ord)
data Month = MkMonth Int deriving (Eq, Show, Ord)
data Year = MkYear Int deriving (Eq, Show, Ord)

addDays :: (Day, Month, Year) -> Int -> (Day, Month, Year)
addDays (MkDay dd,MkMonth mm,MkYear yy) daystoadd =
    let yearType = isLeapYear (yy)
       -- yearDays = yearLength (yy)
       -- yearTypeMonthList = monthLength (mm) (yy)
        --daysRemainingInMonth = leftInMonth (dd,mm,yy)
        --daysSinceStartYear = daysSinceYearBegan (dd, mm,yy)
        --daysLeftInYear = leftInYear (dd,mm, yy)
        newDate = addDaysToDate (MkDay dd,MkMonth mm,MkYear yy) (daystoadd)
    in newDate







isLeapYear :: Int -> Bool
isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


     
--yearLength :: Year -> Day                                        --days
--yearLength n = if (isLeapYear n) then ( MkDay(366) ) else ( MkDay(365) )

yearLength :: Int -> Int                                       --days
yearLength n = if (isLeapYear n) then 366 else 365



monthLength :: Int -> Int -> Int                                --days in a month of a particular year
monthLength mm yy = 
    let months = if isLeapYear yy 
                    then  [31,29,31,30,31,30,31,31,30,31,30,31]
                else [31,28,31,30,31,30,31,31,30,31,30,31]
        monthLengthInDays = months !! (mm-1)
    in monthLengthInDays


leftInMonth :: (Int,Int,Int) -> Int                                --no of days left in a month from current day
leftInMonth (dd,mm,yy) =   (monthLength mm yy) - dd      + 1         


daysSinceYearBegan :: (Int,Int,Int) -> Int                          --total days became since start of year
daysSinceYearBegan (dd,mm,yy) = (sum ( (take (mm-1) ) [monthLength (mm) yy] ) ) + dd



leftInYear :: (Int,Int,Int) -> Int                                      --total days left in year
leftInYear (dd,mm,yy) = yearLength (yy ) - (daysSinceYearBegan (dd,mm,yy) )


addDaysToDate :: (Day, Month, Year) -> Int -> (Day, Month, Year)
--addDaysToDate (dd,mm,yy) (0) = (dd,mm,yy)

addDaysToDate (MkDay dd,MkMonth mm,MkYear yy) (days) = 
                if (days == 0) 
                    then (MkDay dd,MkMonth mm,MkYear yy)
                else if ( (dd==(1) )&& (mm == (1) ) && ( days>=  (yearLength  yy ) ) ) 
                    then addDays (MkDay 1,MkMonth 1,MkYear (yy+1)) ( ( days) -  (yearLength yy )  ) 
                --else if (days>=(yearLength yy))
                    --then addDays (MkDay dd,MkMonth mm,MkYear ( (yy)+ (days `mod` (yearLength yy) ) )  ) (days -(days `mod` (yearLength yy)) )                  
                else  
                    if (  days >= (leftInYear (dd,mm,yy)) ) 
                        then addDays (MkDay dd,MkMonth mm,MkYear (yy+1)) (days- (leftInYear (dd,mm,yy)) )
                    else if ( days >=  (leftInMonth (dd,mm,yy)  ) )
                        then addDays (if mm == (12) then (MkDay 1,MkMonth 1,MkYear (yy+1) ) else (MkDay 1,MkMonth (mm+ 1),MkYear yy) ) (days- ( (leftInMonth (dd,mm,yy) ) ) )
                    else (  MkDay (dd + days ) ,MkMonth mm,MkYear yy)


