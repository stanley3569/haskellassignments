module Chapter3.Pagination2 where

import Data.List

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
 
type NoOfPages = Int 
type PageNoList = String

totalNoOfPages :: TotalItems -> ItemsPerPage -> NoOfPages
totalNoOfPages totalItems itemsPerPage = if( (totalItems `mod` itemsPerPage) == 0)
                                            then ( totalItems `div` itemsPerPage )
                                         else ( ( totalItems `div` itemsPerPage ) + 1)

displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> PageNoList
displayPagination totalItems itemsPerPage currentPage = if((totalNoOfPages totalItems itemsPerPage) < 8 )
                                                            then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages totalItems itemsPerPage) ) ++ " Next>>"
                                                        else 
                                                            if currentPage>8
                                                              then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage  (currentPage+3)    ) ++" | ... | Next>>"
                                                            else "<< Prev |" ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage ( currentPage+3 )    ) ++" | ... | Next>>"
                                                            


pageNumber :: Int -> Int -> Int -> String
pageNumber startPage centerPage endPage= ( concat (intersperse " | " (map show[(startPage)..(centerPage)]) ) ) ++ "* | " ++ ( concat (intersperse " | " (map show[(centerPage+1)..(endPage)]) ) )








checkStartPage :: Int -> Int
checkStartPage value = if(value<1)
                            then 1
                        else value




type NumOfPagesToDisplay = Int 
displayPagination1 :: NumOfPagesToDisplay -> TotalItems -> ItemsPerPage -> CurrentPage -> PageNoList
displayPagination1 noOfPagesToDisplay totalItems itemsPerPage currentPage = if( (totalNoOfPages totalItems itemsPerPage) < noOfPagesToDisplay )
                                                                                then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages totalItems itemsPerPage) ) ++ " Next>>"
                                                                            else
                                                                                if ( ((totalNoOfPages totalItems itemsPerPage)-currentPage) > (endSide noOfPagesToDisplay) && currentPage > ns noOfPagesToDisplay )   
                                                                                    then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-(endSide noOfPagesToDisplay)-1) ) currentPage  (currentPage+(endSide noOfPagesToDisplay)+1)    ) ++" | ... | Next>>"
                                                                                else if ( currentPage > ns noOfPagesToDisplay   )
                                                                                    then "<< Prev | ... " ++ (pageNumber ( checkStartPage((totalNoOfPages totalItems itemsPerPage)-noOfPagesToDisplay+1 ) ) currentPage  ((totalNoOfPages totalItems itemsPerPage))    ) ++" | Next>>"
                                                                                else "<< Prev  " ++ (pageNumber ( checkStartPage(1) ) currentPage  (noOfPagesToDisplay)    ) ++" | ... | Next>>"
    
    
    {-}
    if((totalNoOfPages totalItems itemsPerPage) < noOfPagesToDisplay )
                                                                                   then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages totalItems itemsPerPage) ) ++ " Next>>"
                                                                               else 
                                                                                   if currentPage> noOfPagesToDisplay
                                                                                       then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage  (currentPage+3)    ) ++" | ... | Next>>"
                                                                                   else "<< Prev |" ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage ( currentPage+3 )    ) ++" | ... | Next>>"
   
   -}


endSide noOfPagesToDisplay = (noOfPagesToDisplay `div` 2)-1

ns noOfPagesToDisplay= if (noOfPagesToDisplay `mod` 2) ==0 
                            then noOfPagesToDisplay `div` 2 
                       else (noOfPagesToDisplay `div` 2)+1