{-# LANGUAGE OverloadedStrings #-}


module Practice4 where


import Database.PostgreSQL.Simple
import Text.Read
--import Data.Text as DT
import Control.Monad
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import Prelude hiding (id)
import Data.Time.LocalTime
--import Data.List as DL
--import Data.String.Conv

data Customer = Customer {
    custId :: Integer,
    custCustomerRef :: String,
    custTitle ::String,
    custFullName :: String,
    custEmail :: String,
    custPhone :: String,
    custClientId :: Integer,
    custNumberOfBookings :: Integer,
    custLastBookingCreatedAt :: LocalTime,
    custCreatedAt :: LocalTime,
    custUpdatedAt :: LocalTime
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

        return $ Customer {custId=id1,custCustomerRef=customerRef1,custTitle=title1,custFullName=fullName1,custEmail=email1,custPhone=phone1,custClientId=clientId1,custNumberOfBookings=numberOfBookings1,custLastBookingCreatedAt=lastBookingCreatedAt1,custCreatedAt=createdAt1,custUpdatedAt=updatedAt1}

instance ToRow Customer where
   toRow r = [toField (custId r),toField (custCustomerRef r),toField (custTitle r),toField (custFullName r),toField (custEmail r),toField (custPhone r),toField (custClientId r),toField (custNumberOfBookings r),toField (custLastBookingCreatedAt r),toField (custCreatedAt r), toField(custUpdatedAt r)]



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


    case (readMaybe idGet :: Maybe Integer,readMaybe clientIdGet :: Maybe Integer,readMaybe noOfBookingGet :: Maybe Integer)of
            (Just idGet2,Just clientIdGet2, Just noOfBookingGet2) -> pure $ Customer {custId=(idGet2 :: Integer) ,custCustomerRef=customerRefGet,custTitle=titleGet,custFullName=fullnameGet,custEmail=emailGet,custPhone=phoneGet,custClientId= ( clientIdGet2 :: Integer),custNumberOfBookings=(noOfBookingGet2 :: Integer),custLastBookingCreatedAt= (read lastBookingGet :: LocalTime)  , custCreatedAt=(read createdAtGet:: LocalTime),custUpdatedAt=(read updatedAtGet :: LocalTime)}
            _ -> error "invalid data"



connection :: IO Connection
connection = 
    connect (ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"})




selectQuery1 :: Connection -> IO ()
selectQuery1 conn = do
    print =<< ( query_ conn "select * from customers"  :: IO [Customer])


selectQuery2 :: Connection -> IO ()
selectQuery2 conn = do
    xs <- ( query_ conn "select title from customers"  :: IO [(String)])
    forM_ xs $ \(title1) -> putStrLn $ (title1) 





insertQuery1 :: Connection -> IO ()
insertQuery1 conn = do
    customerData <- getDetails
    result <- execute conn "INSERT INTO customers (id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,last_booking_created_at,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"  customerData
    if (result > 0)
        then putStrLn " Successfully executed the query "
    else 
        putStrLn " Failed: No rows deleted "








updateQuery1 :: Connection -> IO ()
updateQuery1 conn= do

    putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet1 <- getLine
    noOfBookingGet <- case (readMaybe noOfBookingGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"    


    putStrLn "Enter the id of the record which is being updated"
    idGet1 <- getLine
    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"  

    result <- execute conn "update customers SET number_of_bookings=upd.x from (VALUES (?,?)) as upd(x,y) where id=upd.y" ((noOfBookingGet::Integer),(idGet::Integer))

    if (result > 0)
        then putStrLn " Successfully executed the query "
    else 
        putStrLn " Failed: No rows deleted "





updateQuery2 :: Connection -> IO ()
updateQuery2 conn = do

    putStrLn "Enter the customer ref to be set for the update"
    customerRefGet<- getLine

    putStrLn "Enter the title to be set for the update"
    titleGet <- getLine

    putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet1 <- getLine
    noOfBookingGet <- case (readMaybe noOfBookingGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"   


    putStrLn "Enter the id of the record which is being updated"
    idGet1 <- getLine
    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"  

    result <- execute conn "update customers SET customer_ref=upd.a, title=upd.b, number_of_bookings=upd.x from (VALUES (?,?,?,?)) as upd(a,b,x,y) where id=upd.y" (customerRefGet,titleGet, (noOfBookingGet::Integer),(idGet:: Integer))

    if (result > 0)
        then putStrLn " Successfully executed the query "
    else 
        putStrLn " Failed: No rows deleted "



deleteQuery1 :: Connection -> IO ()
deleteQuery1 conn= do
    putStrLn " Enter the id to be deleted"
    idGet1 <- getLine

    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
                Just x -> pure x
                Nothing ->  error "invalid data"  

    result <- execute conn "delete from customers where id= (?) " [((idGet)::Integer)]
    if (result > 0)
        then putStrLn " Successfully executed the query "
    else 
        putStrLn " Failed: No rows deleted "






main :: IO ()
main = do
    conn <- connection
    putStrLn "Enter  \n 1. Insert Query \n 2. Update Query (for one value) \n 3. Delete Query \n 4. Update (for 3 values) \n 5. Select Query \n 6. Exit"
    choice <- getLine
    case ((readMaybe choice)::Maybe Int) of
        Just 1 -> insertQuery1 conn >> main
        Just 2 -> updateQuery1 conn >> main
        Just 3 -> deleteQuery1 conn >> main
        Just 4 -> updateQuery2 conn >> main
        Just 5 -> selectQuery2 conn >> main
        Just 6 -> putStrLn "Exit"
        _ -> main




selectQuery3 ::  IO ()
selectQuery3 = do
    conn <-connection
    xs <- ( query_ conn "select id,title from customers"  :: IO [(Integer,String)])
    forM_ xs $ \(id1,title1) -> putStrLn $ ((title1) ++ " " ++ show (id1))
