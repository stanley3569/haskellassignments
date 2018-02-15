module Chapter3.DateArithmeticNew1 where


--isLeapYear :: Int -> Bool
--isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))


             
--yearLength :: Int -> Int
--yearLength n = if (isLeapYear n) then 366 else 365


--monthLength :: Int -> [Int]
--monthLength yy =  
--    let months = if isLeapYear yy 
--                    then  [31,29,31,30,31,30,31,31,30,31,30,31]
 --               else [31,28,31,30,31,30,31,31,30,31,30,31]
 --   in months
  

--leftInMonth :: (Int,Int,Int) -> Int
--leftInMonth (dd,mm,yy) = ( (monthLength yy) !! (mm-1)  )  - dd     + 1     

  
--daysSinceYearBegan :: (Int,Int,Int) -> Int
--daysSinceYearBegan (dd,mm,yy) =if(mm==1)
--                                then dd
--                               else
--                                dd + sum( take (mm-1)  (monthLength yy) ) 



--leftInYear :: (Int,Int,Int) -> Int
--leftInYear (dd,mm,yy) = yearLength (yy) - (daysSinceYearBegan (dd,mm,yy) ) +1



addDays :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDays (dd,mm,yy) days =  
                            if ( days >= leftInYear (dd,mm,yy))
                                then addDays (1,1,(yy+1)) (days- (leftInYear (dd,mm,yy)) )                            
                            else if ( days >= leftInMonth (dd,mm,yy)  )
                                    then if(mm<=12 ) 
                                            then addDays (dd,mm+1,yy) (days- (leftInMonth (dd,mm,yy) ) )
                                          else addDays (1,1,yy+1) (days- (leftInMonth (dd,mm,yy) ) )                                     
                            else ( (dd + days),mm,yy)
                            where
                                leftInYear (dd',mm',yy') = yearLength yy' - (daysSinceYearBegan (dd',mm',yy') ) +1
                                daysSinceYearBegan (dd1,mm1,yy1) =if(mm1==1)
                                                                    then dd1
                                                                   else
                                                                    dd1 + sum( take (mm1-1)  (monthLength yy1) ) 
                                leftInMonth (dd2,mm2,yy2) = ( (monthLength yy2) !! (mm2-1)  )  - dd2     + 1  
                                monthLength yy2 = if isLeapYear yy2 
                                                    then  [31,29,31,30,31,30,31,31,30,31,30,31]
                                                  else [31,28,31,30,31,30,31,31,30,31,30,31]
                                yearLength n = if (isLeapYear n) then 366 else 365
                                isLeapYear x = ((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))
                                




