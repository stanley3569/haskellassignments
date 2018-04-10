{-# LANGUAGE OverloadedStrings #-}


module Practice3 where


import Database.PostgreSQL.Simple
import Text.Read
--import Data.Text
import Control.Monad
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import Prelude hiding (id)
import Data.Time.LocalTime
import Data.List

data Customer = Customer {
    id :: Integer,
    customerRef :: String,
    title ::String,
    fullName :: String,
    email :: String,
    phone :: String,
    clientId :: Integer,
    numberOfBookings :: Integer,
    lastBookingCreatedAt :: LocalTime,
    createdAt :: LocalTime,
    updatedAt :: LocalTime
} deriving (Show)

instance FromRow Customer where
    fromRow =  do
        id1 <- field
        customerRef1 <- field
        title1 <- field
        fullName1 <- field
        email1 <- field
        phone1 <- field
        clientId1 <- field
        numberOfBookings1 <- field
        lastBookingCreatedAt1 <- field
        createdAt1 <- field
        updatedAt1 <- field

        return $ Customer {id=id1,customerRef=customerRef1,title=title1,fullName=fullName1,email=email1,phone=phone1,clientId=clientId1,numberOfBookings=numberOfBookings1,lastBookingCreatedAt=lastBookingCreatedAt1,createdAt=createdAt1,updatedAt=updatedAt1}

instance ToRow Customer where
   toRow r = [toField (id r),toField (customerRef r),toField (title r),toField (fullName r),toField (email r),toField (phone r),toField (clientId r),toField (numberOfBookings r),toField (lastBookingCreatedAt r),toField (createdAt r), toField(updatedAt r)]



getDetails :: IO Customer
getDetails = do
    putStrLn "Enter the id"
    idGet <- getLine

    putStrLn "Enter thee customer ref"
    customerRefGet<- getLine

    putStrLn "Enter the title"
    titleGet <- getLine
    
    putStrLn "Enter the fullname"
    fullnameGet <- getLine

    putStrLn "Enter the email"
    emailGet <- getLine

    putStrLn "Enter the phone"
    phoneGet <- getLine

    putStrLn "Enter the client id"
    clientIdGet <- getLine

    putStrLn "Enter the number of booking"
    noOfBookingGet <- getLine

    putStrLn "Enter the last booking created at"
    lastBookingGet <- getLine

    putStrLn "Enter the created at"
    createdAtGet <- getLine

    putStrLn "Enter the updated at"
    updatedAtGet <- getLine

    pure $ Customer {id=(read idGet :: Integer) ,customerRef=customerRefGet,title=titleGet,fullName=fullnameGet,email=emailGet,phone=phoneGet,clientId= (read clientIdGet :: Integer),numberOfBookings=(read noOfBookingGet :: Integer),lastBookingCreatedAt= (read lastBookingGet :: LocalTime)  , createdAt=(read createdAtGet:: LocalTime),updatedAt=(read updatedAtGet :: LocalTime)}







connection :: IO Connection
connection = 
    connect (ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"})




selectQuery1 :: IO [Customer]
selectQuery1 = do
    conn <- connection
    ( query_ conn "select * from customers"  :: IO [Customer])


selectQuery2 :: IO ()
selectQuery2 = do
    conn <- connection
    dataV <- (query_ conn "select * from customers"  :: IO [Customer])
    displayData <- pure (foldl' (\arr Customer{id=_,customerRef=_,title=_,fullName=_,email=x,phone=_,clientId=_,numberOfBookings=_,lastBookingCreatedAt=_,createdAt=_,updatedAt=_} -> arr++x++"\n") "" dataV)
    putStrLn displayData




insertQuery1 :: IO Int64
insertQuery1 = do
    customerData <- getDetails
    conn <- connection
    execute conn "INSERT INTO customers (id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,last_booking_created_at,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"  customerData







updateQuery1 :: IO Int64
updateQuery1 = do
    conn <- connection

    putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet <- getLine

    putStrLn "Enter the id of the record which is being updated"
    idGet <- getLine

    execute conn "update customers SET number_of_bookings=upd.x from (VALUES (?,?)) as upd(x,y) where id=upd.y" ((read noOfBookingGet::Integer),(read idGet::Integer))





updateQuery2 :: IO Int64
updateQuery2 = do
    conn <- connection

    putStrLn "Enter thee customer ref to be set for the update"
    customerRefGet<- getLine

    putStrLn "Enter the title to be set for the update"
    titleGet <- getLine

    putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet <- getLine

    putStrLn "Enter the id of the record which is being updated"
    idGet <- getLine

    execute conn "update customers SET customer_ref=upd.a, title=upd.b, number_of_bookings=upd.x from (VALUES (?,?,?,?)) as upd(a,b,x,y) where id=upd.y" (customerRefGet,titleGet, (read noOfBookingGet::Integer),(read idGet::Integer))





deleteQuery1 :: IO Int64
deleteQuery1 = do
    conn <- connection

    putStrLn " Enter the id to be deleted"
    idGet <- getLine

    execute conn "delete from customers from  where id= (?) " [((read idGet)::Integer)]






main :: IO ()
main = do
    putStrLn "Enter  \n 1. Insert Query \n 2. Update Query (for one value) \n 3. Delete Query \n 4. Update (for 3 values) \n 5. Select Query \n 6. Exit"
    choice <- getLine
    case ((readMaybe choice)::Maybe Int) of
        Just 1 -> insertQuery1 >> main
        Just 2 -> updateQuery1 >> main
        Just 3 -> deleteQuery1 >> main
        Just 4 -> updateQuery2 >> main
        Just 5 -> selectQuery2 >> main
        Just 6 -> putStrLn "Exit"
        _ -> main



