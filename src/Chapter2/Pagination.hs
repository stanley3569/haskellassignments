module Chapter2.Pagination where

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
pageNumber startPage centerPage endPage= (foldl' (\xs x -> xs ++ " | "++ (show x)) "" [startPage..centerPage])  ++   (foldl' (\ xs x -> xs ++ " | "++ (show x)) "*"  [(centerPage+1)..endPage] )








checkStartPage :: Int -> Int
checkStartPage value = if(value<1)
                            then 1
                        else value


--checkCurrentPage :: Int -> Int -> Int
--checkCurrentPage totalPages value = if(value> (value-1))
 --                                       then (value-1)
   --                                 else value



type NumOfPagesToDisplay = Int 
displayPagination1 :: NumOfPagesToDisplay -> TotalItems -> ItemsPerPage -> CurrentPage -> PageNoList
displayPagination1 noOfPagesToDisplay totalItems itemsPerPage currentPage= if((totalNoOfPages totalItems itemsPerPage) < noOfPagesToDisplay )
                                                                                   then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages totalItems itemsPerPage) ) ++ " Next>>"
                                                                               else 
                                                                                   if currentPage> noOfPagesToDisplay
                                                                                       then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage  (currentPage+3)    ) ++" | ... | Next>>"
                                                                                   else "<< Prev |" ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage ( currentPage+3 )    ) ++" | ... | Next>>"
   
   


endSide noOfPagesToDisplay = (noOfPagesToDisplay `div` 2)-1

ns noOfPagesToDisplay= if (noOfPagesToDisplay `mod` 2) ==0 then noOfPagesToDisplay `div` 2 else (noOfPagesToDisplay `div` 2)+1