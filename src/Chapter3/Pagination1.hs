module Chapter3.Pagination1 where

import Data.List

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
 
type NoOfPages = Int 
type PageNoList = String


data PaginationTotalItems = MkTotalItems Int deriving (Eq, Show,Ord)
data PaginationItemsPerPage = MkItemsPerPage Int deriving (Eq, Show,Ord)



totalNoOfPages :: PaginationTotalItems -> PaginationItemsPerPage -> NoOfPages
totalNoOfPages (MkTotalItems totalItems) (MkItemsPerPage itemsPerPage )= 
                                         if( (totalItems `mod` itemsPerPage) == 0)
                                            then ( totalItems `div` itemsPerPage )
                                          else ( ( totalItems `div` itemsPerPage ) + 1)

displayPagination :: PaginationTotalItems -> PaginationItemsPerPage -> CurrentPage -> PageNoList
displayPagination (MkTotalItems totalItems) (MkItemsPerPage itemsPerPage) currentPage =
                                                         if((totalNoOfPages (MkTotalItems totalItems) (MkItemsPerPage itemsPerPage) ) < 8 )
                                                            then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages (MkTotalItems totalItems) (MkItemsPerPage itemsPerPage) ) ) ++ " Next>>"
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