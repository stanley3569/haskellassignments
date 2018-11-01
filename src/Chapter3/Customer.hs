module Chapter3.Customer where

import Data.List (dropWhile, dropWhileEnd)
import Data.Char (isSpace)

data Customer = MkCustomer{
    customerId :: Int,
    customerFirstName :: String,
    customerLastName :: String,
    customerEmail :: String,
    customerBirthYear :: Int,
    customerCity :: String,
    customerCountry :: String
} deriving (Eq, Show, Ord)



removeSpaces :: String -> String
removeSpaces s = dropWhileEnd (\x -> isSpace x) (dropWhile (\x -> isSpace x) s)


cleanupName :: Customer -> Customer
cleanupName MkCustomer { customerId = cid, customerFirstName = fn, customerLastName = ln, customerEmail = e, customerBirthYear = by, customerCity = cy, customerCountry = co } = 
        MkCustomer
            {
                customerId = cid,
                customerFirstName = (removeSpaces fn),
                customerLastName = (removeSpaces ln),
                customerEmail = e,
                customerBirthYear = by,
                customerCity = cy,
                customerCountry = co
            }                





{-
cleanupName MkCustomer { customerId = 123 , customerFirstName = "    stan", customerLastName ="  fern",customerEmail = "sta@vl.com", customerBirthYear = 2018, customerCity = "mapusa", customerCountry = "India" }

MkCustomer {customerId = 123, customerFirstName = "stan", customerLastName = "fern", customerEmail = "sta@vl.com", customerBirthYear = 2018, customerCity = "mapusa", customerCountry = "India"}
-}


cleanupName1 :: Customer -> Customer
cleanupName1 cust@MkCustomer {customerFirstName = fn, customerLastName = ln} =
    cust
    {
        customerFirstName = (removeSpaces fn),
        customerLastName = (removeSpaces ln)

    }



cleanupName2 :: Customer -> Customer
cleanupName2 cust =
  cust
    { 
    customerFirstName = (removeSpaces (customerFirstName cust)),
    customerLastName = (removeSpaces (customerLastName cust))
    }