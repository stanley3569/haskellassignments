module Chapter3.CalculateEndTime2 where

data Date = MkDate {                        --Date is new record type --MkDate is the constructor
    dtDay :: Int,                           -- dtDay, dtMonth, dtYear are the named fields    
    dtMonth :: Int,
    dtYear :: Int
} deriving (Eq, Show, Ord)

--let d = MkDate {dtDay = 1, dtMonth = 1 , dtYear = 2017 }





