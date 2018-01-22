module Chapter3.Pagination where

import Data.List

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
 
type NoOfPages = Int 
type PageNoList = String



data Items = MkItems {totalItems::Int , itemsPerPage::Int} deriving (Eq, Show,Ord)



totalNoOfPages :: Items -> NoOfPages
totalNoOfPages (MkItems totalItems itemsPerPage) =
                                let totalpages = if( (totalItems `mod` itemsPerPage) == 0)
                                                     then ( totalItems `div` itemsPerPage )
                                                 else ( ( totalItems `div` itemsPerPage ) + 1)
                                    in totalpages

displayPagination :: Items -> CurrentPage -> PageNoList
displayPagination (MkItems totalItems itemsPerPage) currentPage =
                                    let displaypages =   if((totalNoOfPages (MkItems totalItems itemsPerPage) ) < 8 )
                                                            then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages (MkItems totalItems itemsPerPage)) ) ++ " Next>>"
                                                        else 
                                                            if currentPage>8
                                                              then "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage  ( checkEndPage  (currentPage+3) (totalNoOfPages (MkItems totalItems itemsPerPage) )      )    ) ++" | ... | Next>>"
                                                            else "<< Prev |" ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage ( checkEndPage  (currentPage+3) (totalNoOfPages (MkItems totalItems itemsPerPage) )      )     ) ++" | ... | Next>>"
                                    in displaypages


pageNumber :: Int -> Int -> Int -> PageNoList
pageNumber startPage centerPage endPage= ( concat (intersperse " | " (map show[(startPage)..(centerPage)]) ) ) ++ "* | " ++ ( concat (intersperse " | " (map show[(centerPage+1)..(endPage)]) ) )





checkStartPage :: Int -> Int
checkStartPage value = if(value<1)
                            then 1
                        else value



checkEndPage :: Int -> Int -> Int 
checkEndPage pageNo endPage = if(pageNo> endPage)
                                    then endPage
                               else pageNo