{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DbUI where



import Lucid
import Data.Monoid
import Lucid.Base


import Database.PostgreSQL.Simple
import Text.Read
import Control.Monad
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import Prelude hiding (id)
import Data.Time.LocalTime
import Data.List as DL
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader





import Prelude 
--import Prelude.Compat

import Control.Monad.Except
--import Control.Monad.Reader
--import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html











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


selectQuery2 :: ReaderT Connection IO() 
selectQuery2 = do
    conn <- ask
    xs <- liftIO $  query_ conn "select id,title from customers"  :: ReaderT Connection  IO [(Integer,String)]
    x <- pure $ mapM_ (\(id1,title1) -> putStrLn $ (title1 ++ " "++  (show id1) ) ) xs
    liftIO x



insertQuery1 :: ReaderT Connection IO () 
insertQuery1 = do
    conn <- ask
    customerData <- liftIO $ getDetails
    result <- liftIO $ execute conn "INSERT INTO customers (id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,last_booking_created_at,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"  customerData
    if (result > 0)
        then liftIO $ putStrLn " Successfully executed the query "
    else 
        liftIO $ putStrLn " Failed: No rows deleted "



updateQuery1 :: ReaderT (Connection) IO () 
updateQuery1 = do
    conn <- ask
    liftIO $ putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet1 <- liftIO $ getLine
    noOfBookingGet <- case (readMaybe noOfBookingGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"    


    liftIO $ putStrLn "Enter the id of the record which is being updated"
    idGet1 <- liftIO $ getLine
    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"  

    result <- liftIO $ execute conn "update customers SET number_of_bookings=upd.x from (VALUES (?,?)) as upd(x,y) where id=upd.y" ((noOfBookingGet::Integer),(idGet::Integer))

    if (result > 0)
        then liftIO $ putStrLn " Successfully executed the query "
    else 
        liftIO $ putStrLn " Failed: No rows deleted "





updateQuery2 :: ReaderT (Connection) IO () 
updateQuery2 = do
    conn <- ask
    liftIO $ putStrLn "Enter the customer ref to be set for the update"
    customerRefGet<- liftIO $ getLine

    liftIO $ putStrLn "Enter the title to be set for the update"
    titleGet <- liftIO $ getLine

    liftIO $ putStrLn "Enter the number of booking to be set for the update"
    noOfBookingGet1 <- liftIO $ getLine
    noOfBookingGet <- case (readMaybe noOfBookingGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"   


    liftIO $ putStrLn "Enter the id of the record which is being updated"
    idGet1 <- liftIO $ getLine
    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
        Just x -> pure x
        Nothing ->  error "invalid data"  

    result <- liftIO $ execute conn "update customers SET customer_ref=upd.a, title=upd.b, number_of_bookings=upd.x from (VALUES (?,?,?,?)) as upd(a,b,x,y) where id=upd.y" (customerRefGet,titleGet, (noOfBookingGet::Integer),(idGet:: Integer))

    if (result > 0)
        then liftIO $ putStrLn " Successfully executed the query "
    else 
        liftIO $ putStrLn " Failed: No rows deleted "

 

deleteQuery1 :: ReaderT (Connection) IO () 
deleteQuery1 = do
    conn <- ask
    liftIO $ putStrLn " Enter the id to be deleted"
    idGet1 <- liftIO $ getLine

    idGet <- case (readMaybe idGet1) :: (Maybe Integer) of
                Just x -> pure x
                Nothing ->  error "invalid data"  

    result <- liftIO $ execute conn "delete from customers where id= (?) " [((idGet)::Integer)]
    if (result > 0)
        then liftIO $ putStrLn " Successfully executed the query "
    else 
        liftIO $ putStrLn " Failed: No rows deleted "






main :: IO ()
main = do
    conn <- connection
    putStrLn "Enter  \n 1. Insert Query \n 2. Update Query (for one value) \n 3. Delete Query \n 4. Update (for 3 values) \n 5. Select Query \n 6. Exit"
    choice <- getLine
    case ((readMaybe choice)::Maybe Int) of
        Just 1 -> runReaderT insertQuery1 conn >> main
        Just 2 -> runReaderT updateQuery1 conn >> main
        Just 3 -> runReaderT deleteQuery1 conn >> main
        Just 4 -> runReaderT updateQuery2 conn >> main
        Just 5 -> runReaderT selectQuery2 conn >> main
        Just 6 -> putStrLn "Exit"
        _ -> main






selectQuery3 :: ReaderT Connection IO [(Integer, String)]
selectQuery3 = do
    conn <- ask
    x <- liftIO $ query_ conn "select id,title from customers"  :: ReaderT Connection  IO [(Integer,String)]
    liftIO $ pure x 


displayConv :: IO (Html())
displayConv = do
    conn <- connection
    fmap (\x -> displayQueryUI x ) (runReaderT selectQuery3 conn) 



displayQueryUI :: [(Integer, String)] -> Html()
displayQueryUI queryResult = do
    doctypehtml_
        (do head_
                (do title_ "Display"
                    link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                    style_ "body {background:lightblue")
            body_
                (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                    table_ (tr_ (do td_ "id"
                                    td_ "title"
                                    mapM_ (\(x,y) ->tr_ $ do td_ (toHtml (show x) ) 
                                                             td_ (toHtml y)   ) ( queryResult)  ) )   ))





insertQueryUI:: Html()
insertQueryUI =
    doctypehtml_
            (do head_
                    (do title_ "Display"
                        link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                        style_ "body {background:lightblue")
                body_
                    (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                        form_ [id_ "customerForm"]
                            (do label_ "Customer Id"
                                input_ [type_ "text",name_ "id"]
                                br_ [class_ "formBreak"]
                                
                                label_ "Customer Reference"
                                input_ [type_ "text",name_ "customerRef"]
                                br_ [class_ "formBreak"]

                                label_ "Title"
                                input_ [type_ "text",name_ "title"]                                
                                br_ [class_ "formBreak"]

                                label_ "Fullname"
                                input_ [type_ "text",name_ "fullName"]
                                br_ [class_ "formBreak"]

                                label_ "Email"
                                input_ [type_ "text",name_ "email"]
                                br_ [class_ "formBreak"]

                                label_ "Phone"
                                input_ [type_ "text",name_ "phone"]                                
                                br_ [class_ "formBreak"]

                                label_ "Client Id"
                                input_ [type_ "text",name_ "clientId"]                                
                                br_ [class_ "formBreak"]

                                label_ "No. of Booking"
                                input_ [type_ "text",name_ "noOfBooking"]                                
                                br_ [class_ "formBreak"]

                                label_ "Last booking created at"
                                input_ [type_ "datetime-local",name_ "lastBooking"]                                
                                br_ [class_ "formBreak"]

                                label_ "Last booking created at"
                                input_ [type_ "datetime-local",name_ "lastBooking"]  
                                br_ [class_ "formBreak"]

                                label_ "Created at"
                                input_ [type_ "datetime-local",name_ "createdAt"]  
                                br_ [class_ "formBreak"]

                                label_ "Updated at"
                                input_ [type_ "datetime-local",name_ "updateAt"]  
                                br_ [class_ "formBreak"]

                                input_ [type_ "submit",name_ "Submit",value_ "Submit"]

                                )     ) ) 





updateQueryUI:: Html()
updateQueryUI =
    doctypehtml_
        (do head_
                (do title_ "Update"
                    link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                    style_ "body {background:lightblue")
            body_
                (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                    form_ [id_ "updateForm"]
                        (do label_ "No. of Booking"
                            input_ [type_ "text",name_ "noOfBooking"]                                
                            br_ [class_ "formBreak"]
                            
                            label_ "Customer Id"
                            input_ [type_ "text",name_ "id"]
                            br_ [class_ "formBreak"]    

                            input_ [type_ "submit",name_ "Submit",value_ "Submit"]
                            
                                                            )     ) ) 

deleteQueryUI:: Html()
deleteQueryUI =
    doctypehtml_
            (do head_
                    (do title_ "Delete"
                        link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                        style_ "body {background:lightblue")
                body_
                    (do div_ [id_ "header",style_ "color:white"] "Delete Customer"
                        form_ [id_ "customerForm"]
                            (do label_ "Customer Id"
                                input_ [type_ "text",name_ "id"]
                                br_ [class_ "formBreak"]

                                input_ [type_ "submit",name_ "Submit",value_ "Submit"]                                
                                                                )     ) ) 