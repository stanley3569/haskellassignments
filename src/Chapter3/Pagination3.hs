module Chapter3.Pagination3 where

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
                                                              then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage  ( checkEndPage  (currentPage+3) (totalNoOfPages totalItems itemsPerPage)      )    ) ++" | ... | Next>>"
                                                            else "<< Prev |" ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage ( checkEndPage  (currentPage+3) (totalNoOfPages totalItems itemsPerPage)      )    ) ++" | ... | Next>>"
                                                            


pageNumber :: Int -> Int -> Int -> String
pageNumber startPage centerPage endPage = ( concat (intersperse " | " (map show[(startPage)..(centerPage)]) ) ) ++ "* | " ++ ( concat (intersperse " | " (map show[(centerPage+1)..(endPage)]) ) )


checkStartPage :: Int -> Int
checkStartPage value = if(value<1)
                            then 1
                        else value


checkEndPage :: Int -> Int -> Int 
checkEndPage pageNo endPage = if(pageNo> endPage)
                                    then endPage
                               else pageNo
