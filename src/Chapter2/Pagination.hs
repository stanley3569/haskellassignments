module Chapter2.Pagination where

import Data.List

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
 
totalNoOfPages :: Int -> Int -> Int
totalNoOfPages totalItems itemsPerPage = if( (totalItems `mod` itemsPerPage) == 0)
                                            then ( totalItems `div` itemsPerPage )
                                         else ( ( totalItems `div` itemsPerPage ) + 1)

displayPagination :: Int -> Int -> Int -> String
displayPagination totalItems itemsPerPage currentPage = if((totalNoOfPages totalItems itemsPerPage) < 8 )
                                                            then "<<Prev " ++ ( pageNumber 1 currentPage (totalNoOfPages totalItems itemsPerPage) ) ++ " Next>>"
                                                        else  "<< Prev | ... " ++ (pageNumber ( checkStartPage(currentPage-3) ) currentPage (currentPage+3) ) ++" | ... | Next>>"



pageNumber :: Int -> Int -> Int -> String
pageNumber startPage centerPage endPage= (foldl' (\xs x -> xs ++ " | "++ (show x)) "" [startPage..centerPage])  ++   (foldl' (\ xs x -> xs ++ " | "++ (show x)) "*"  [(centerPage+1)..endPage] )






checkStartPage :: Int -> Int
checkStartPage value = if(value<1)
                            then 1
                        else value
