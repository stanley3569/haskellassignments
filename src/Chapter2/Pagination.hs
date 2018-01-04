module Chapter2.Pagination where

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
 
totalNoOfPages :: Int -> Int -> Int
totalNoOfPages TotalItems ItemsPerPage = if( (TotalItems `mod` ItemsPerPage) == 0)
                                            then ( TotalItems `div` ItemsPerPage )
                                         else ( ( TotalItems `div` ItemsPerPage ) + 1)

displayPagination :: Int -> Int -> Int -> String
displayPagination TotalItems ItemsPerPage CurrentPage =