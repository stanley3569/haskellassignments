module Chapter3.Date where
import Data.List



--addDays :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDays (dd,mm,yy) daystoadd =
    let yearType =  isLeapYear yy
        sumYear = (sum yearType)
        daysGone =sum ( (take (mm-1) ) yearType ) + dd
        totalDays = daysGone   + daystoadd
        yearsOverflow = totalDays `div` sumYear                                                     --if totalDays `div` sumYear > 0 then totalDays `div` sumYear else 0
        ------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
        totalDays1 = totalDays `mod` sumYear
        
        
        year = yy+yearsOverflow

                                                                                                    --yearType1 = takeWhile ( (<=totalDays) . sum) ( inits  yearType )
        monthsGone =last ( ( takeWhile (\x -> sum x <=totalDays1) ((inits(yearType) ) ) ) )


        daysofMonth = if (totalDays1 - (sum monthsGone) == 0) 
                            then ( yearType!!(mm-1) ) 
                      else if (totalDays1 <= (yearType!!(mm-1)) )               --(yearType!!(mm-1)) )                        --days
                            then totalDays1 
                      else (totalDays1 - (sum monthsGone) )


                                                                                                        --noofMonths = if ( ((length monthsGone)+1 )>12) 
                                                                                                            -- then ((length monthsGone)+1-12 ) 
                                                                                                                 -- else ( (length monthsGone)+1 )
                                                                                                             --test1 = monthsGone
        
        noofMonths = if (monthsGone/= [])
                        then if ( ((length monthsGone) )>12) 
                                then ((length monthsGone)+1-12 ) 
                            else if( (last monthsGone) == daysofMonth )
                                then ( (length monthsGone) )
                                                                                        --else if((last monthsGone == 30))                                                                                                   --then ( (length monthsGone)+2 )
                            else ( (length monthsGone)+1 )
                                
                     else  (length monthsGone) +1                                                  -- +2
        

                                                                                                 --test2 =  if (totalDays <= (yearType!!(mm-1)) )           --days
                                                                                                    --            then totalDays 
                                                                                                    ---       else (totalDays `mod` (yearType!!(mm-1)))
                                                                                                    --in monthsGone
    in( (daysofMonth,noofMonths, year) , monthsGone,daysGone)                                                            --, test1)



isLeapYear :: Int -> [Int]
isLeapYear x = if((x `mod` 4 ==0) && ((x `mod` 100 /=0)||(x `mod` 400 ==0)))
                then [31,29,31,30,31,30,31,31,30,31,30,31]
             else [31,28,31,30,31,30,31,31,30,31,30,31]
    

