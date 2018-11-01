module Lens.Date1 where

import Control.Lens



addDays :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDays date1 days =  
                            if ( days >= leftInYear date1  )
                                then addDays (1,1,( (date1^._3)  +1)) (days- (leftInYear date1  ) )                            
                            else if ( days >= leftInMonth date1  )
                                    then if((view _2 date1) <=12 ) 
                                            then addDays ( (date1^._1)  ,(date1^._2) +1, (date1^._3) ) (days- (leftInMonth date1 ) )
                                          else addDays (1,1,(view _3 date1)+1) (days- (leftInMonth date1 ) )                                     
                            else ( ( (date1^._1) + days),(date1^._2),(date1^._3))
                            



leftInYear :: (Int,Int,Int) -> Int
leftInYear givenDate = yearLength (givenDate^._3) - (daysSinceYearBegan givenDate ) +1

daysSinceYearBegan :: (Int,Int,Int) -> Int
daysSinceYearBegan givenDate1 =if( (givenDate1^._2)==1)
                                        then (givenDate1^._1)
                                   else ((givenDate1^._1) + sum( take ((givenDate1^._2)-1)  (monthLength (givenDate1^._3)) ) )
                                

leftInMonth :: (Int,Int,Int) -> Int                                   
leftInMonth givenDate2 = ( (monthLength (givenDate2^._3) ) !! ( (givenDate2^._2)-1)  )  - (givenDate2^._1)     + 1  
                                                        
monthLength :: Int -> [Int]
monthLength yy2 = if isLeapYear yy2 
                        then  [31,29,31,30,31,30,31,31,30,31,30,31]
                  else [31,28,31,30,31,30,31,31,30,31,30,31]
         
yearLength :: Int -> Int
yearLength n = if (isLeapYear n) then 366 else 365
     

isLeapYear :: Int -> Bool
isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))
                                

