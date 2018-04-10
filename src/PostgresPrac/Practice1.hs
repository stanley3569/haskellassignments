{-# LANGUAGE OverloadedStrings #-}


module Practice1 where


import Database.PostgreSQL.Simple
import Data.Text
import Control.Monad
import Database.PostgreSQL.Simple.FromRow
import GHC.Int

data Customer = Customer {
    title ::  String,
    email :: String
    } deriving (Show)


instance FromRow Customer where
    fromRow =  do
        title1 <- field
        email1 <- field
        return $ Customer {title=title1,email=email1}



connection :: IO Connection
connection = 
    connect (ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"})

selectQuery :: IO [Customer]
selectQuery = do
    conn <- connection
    query_ conn "SELECT title,email FROM customers" :: IO [Customer]


    
insertQuery :: IO GHC.Int.Int64
insertQuery = do
    conn <- connection
    execute_ conn "INSERT INTO customers (id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,last_booking_created_at,created_at,updated_at) VALUES (1001, 'ad2','asst manager','dexter','dexter123@gmail.com','1234555555',101,11,'2018-04-09 16:21:46.233207','2018-04-09 16:21:46.233207','2018-04-09 16:21:46.233207')"
    


updateQuery :: IO Int64
updateQuery = do
    conn <- connection
    execute_ conn "update customers SET number_of_bookings=55 where id=1001"

deleteQuery :: IO Int64
deleteQuery = do
    conn <- connection
    execute_ conn " delete from customers where id=1000"

















