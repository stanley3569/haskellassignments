module Chapter3.DateArithmeticNew4 where
    
import Data.List

data Date = MkDate {day :: Int, month :: Int, year :: Int } deriving (Eq, Show, Ord)

isLeapYear :: Int -> Bool
isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


             
yearLength :: Int -> Int
yearLength n = if (isLeapYear n) then 366 else 365


monthLength :: Int -> Int -> [Int]
monthLength mm yy =  
    let months = if isLeapYear yy 
                    then  [31,29,31,30,31,30,31,31,30,31,30,31]
                else [31,28,31,30,31,30,31,31,30,31,30,31]
    in months
  

leftInMonth :: Date -> Int
leftInMonth (MkDate dd mm yy) = ( (monthLength mm yy) !! (mm-1)  )  - dd     + 1     

  
daysSinceYearBegan :: Date -> Int
daysSinceYearBegan (MkDate dd mm yy) =if(mm==1)
                                then dd
                               else
                                dd + sum( take (mm-1)  (monthLength mm yy) ) 



leftInYear :: Date -> Int
leftInYear (MkDate dd mm yy) = yearLength (yy) - (daysSinceYearBegan (MkDate dd mm yy) ) +1



addDays :: Date -> Int -> Date
addDays (MkDate dd mm yy) days =  
                            if ( days >= leftInYear (MkDate dd mm yy))
                                then addDays (MkDate 1 1 (yy+1)) (days- (leftInYear (MkDate dd mm yy)) )                            
                            else if ( days >= leftInMonth (MkDate dd mm yy)  )
                                    then if((mm)<=12 ) 
                                            then addDays (MkDate dd (mm+1) yy) (days- (leftInMonth (MkDate dd mm yy) ) )
                                          else addDays (MkDate 1 1 (yy+1) ) (days- (leftInMonth (MkDate dd mm yy) ) )                                     
                            else (MkDate (dd + days) mm yy)

