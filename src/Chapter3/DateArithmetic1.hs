module Chapter3.DateArithmetic1 where
    
import Data.List
addDays :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDays (dd,mm,yy) daystoadd =
        let yearType = isLeapYear yy
            yearDays = yearLength yy
            yearTypeMonthList = monthLength mm yy
            daysRemainingInMonth = leftInMonth (dd,mm,yy)
            daysSinceStartYear = daysSinceYearBegan (dd,mm,yy)
            daysLeftInYear = leftInYear (dd,mm,yy)
            newDate = addDaysToDate (dd,mm,yy) daystoadd
        in newDate







isLeapYear :: Int -> Bool
isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


             
yearLength :: Int -> Int
yearLength n = if (isLeapYear n) then 366 else 365


monthLength :: Int -> Int -> Int
monthLength mm yy = months !! (mm-1) where
    months = if isLeapYear yy then months2 else months1
    months1 = [31,28,31,30,31,30,31,31,30,31,30,31]
    months2 = [31,29,31,30,31,30,31,31,30,31,30,31]
  
  
leftInMonth :: (Int,Int,Int) -> Int
leftInMonth (dd,mm,yy) = (monthLength mm yy) - dd      + 1         
  
  
daysSinceYearBegan :: (Int,Int,Int) -> Int
daysSinceYearBegan (dd,mm,yy) = (sum ( (take (mm-1) ) [monthLength mm yy] ) ) + dd



leftInYear :: (Int,Int,Int) -> Int
leftInYear (dd,mm,yy) = yearLength (yy ) - (daysSinceYearBegan (dd,mm,yy) )


addDaysToDate :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDaysToDate (dd,mm,yy) 0 = (dd,mm,yy)

addDaysToDate (dd,mm,yy) days =
                        if (days == 0) 
                            then (dd,mm,yy)
                        else if ( (dd==1) && (mm == 1) && (days>= yearLength yy) )
                            then addDays (1,1,(yy+1)) (days- yearLength yy)                            
                        else  
                            if ( days >= leftInYear (dd,mm,yy))
                                then addDays (dd,mm,(yy+1)) (days- (leftInYear (dd,mm,yy)) )
                            else if ( days >= leftInMonth (dd,mm,yy)  )
                                then addDays (if mm == 12 then (1,1,(yy+1) ) else (1,(mm+1),yy) ) (days- (leftInMonth (dd,mm,yy) ) )
                            else ( (dd + days),mm,yy)
