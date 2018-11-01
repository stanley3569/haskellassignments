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

module Main1 where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Lucid
import Servant.HTML.Lucid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Time.LocalTime
import Data.List
import Text.Read
import GHC.Int
import GHC.Generics
import Control.Monad.Reader as CM
import Prelude hiding (id)
import Web.FormUrlEncoded
import Control.Monad.IO.Class
class Monad m => MonadIO m where
   liftIO :: IO a -> m a


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
} deriving (Eq, Show, Generic)

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


--instance ToJSON Customer 

--instance FromJSON Customer


instance FromForm Customer where
    fromForm v = do 
        id1 <- (parseUnique "custId" v)
        customerRef1 <- (parseUnique "custCustomerRef" v)
        title1 <- (parseUnique "custTitle" v)
        fullName1 <- (parseUnique "custFullName" v)
        email1 <- (parseUnique "custEmail" v)
        phone1 <- (parseUnique "custPhone" v)
        clientId1 <- (parseUnique "custClientId" v)
        numberOfBookings1 <- (parseUnique "custNumberOfBookings" v)
        lastBookingCreatedAt1 <- (parseUnique "custLastBookingCreatedAt" v)
        createdAt1 <- (parseUnique "custCreatedAt" v)
        updatedAt1 <- (parseUnique "custUpdatedAt" v)
        pure (Customer {custId=id1,custCustomerRef=customerRef1,custTitle=title1,custFullName=fullName1,custEmail=email1,custPhone=phone1,custClientId=clientId1,custNumberOfBookings=numberOfBookings1,custLastBookingCreatedAt=lastBookingCreatedAt1,custCreatedAt=createdAt1,custUpdatedAt=updatedAt1})
    
----------------------------------------------------
data DeleteCustomer = DeleteCustomer {
    custIdDel :: Integer
    } deriving (Eq, Show, Generic)

instance FromRow DeleteCustomer where
    fromRow =  do
        id1 <- field
        return $ DeleteCustomer {custIdDel=id1}

instance ToRow DeleteCustomer where
   toRow r = [toField (custIdDel r)]

instance FromForm DeleteCustomer where
    fromForm v = do 
        id1 <- (parseUnique "custIdDel" v)
        pure (DeleteCustomer {custIdDel=id1})
        
-----------------------------------------
data UpdateCustomer = UpdateCustomer {
    custIdUpdate :: Integer,
    custNumberOfBookingsUpdate :: Integer
    } deriving (Eq, Show, Generic)

instance FromRow UpdateCustomer where
    fromRow =  do
        id1 <- field
        noBooking <- field
        return $ UpdateCustomer {custIdUpdate=id1, custNumberOfBookingsUpdate=noBooking}

instance ToRow UpdateCustomer where
   toRow r = [toField (custIdUpdate r),toField (custNumberOfBookingsUpdate r)]

instance FromForm UpdateCustomer where
    fromForm v = do 
        id1 <- (parseUnique "custIdUpdate" v)
        noBooking <- (parseUnique "custNumberOfBookingsUpdate" v)
        pure (UpdateCustomer {custIdUpdate=id1,custNumberOfBookingsUpdate=noBooking})
        
-------------------------------------------------------


--2018-04-26 12:33:25.523453

type API =  "index" :> PageAPI
            :<|> "index" :> "insertCust" :> InsertAPI
            :<|> "insert" :> ReqBody '[FormUrlEncoded] Customer :> Post '[HTML] (String)
            :<|> "index" :> "deleteCust" :> DeleteAPI
            :<|> "delete" :> ReqBody '[FormUrlEncoded] DeleteCustomer :> Post '[HTML] (String)
            :<|> "index" :> "updateCust" :> UpdateAPI
            :<|> "update" :> ReqBody '[FormUrlEncoded] UpdateCustomer :> Post '[HTML] (String)
            :<|> "index" :> "displayCust" :> DisplayAPI

           
server :: Server API
server = pageServer :<|> insertServer :<|> customerInsert :<|> deleteServer :<|> customerDelete  :<|> updateServer :<|> customerUpdate  :<|> customerDisplay        

customerInsert :: Customer -> Handler (String)
customerInsert cust = do
                        conn <- CM.liftIO $ connection    
                        result <- CM.liftIO $ execute conn "INSERT INTO customers (id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,last_booking_created_at,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"  cust
                        if (result > 0)
                            then  pure " Successfully executed the query "
                        else 
                            pure " Failed: No rows deleted "


customerDelete :: DeleteCustomer -> Handler (String)
customerDelete idGet = do
                        conn <- CM.liftIO $ connection   
                        result <- CM.liftIO $ execute conn "delete from customers where id= (?) " (idGet)
                        if (result > 0)
                            then pure" Successfully executed the query "
                        else 
                            pure " Failed: No rows deleted "            
                    
                
customerUpdate :: UpdateCustomer -> Handler (String)
customerUpdate upVal = do
        conn <- CM.liftIO $ connection
        result <- CM.liftIO $ execute conn "update customers SET number_of_bookings=upd.y from (VALUES (?,?)) as upd(x,y) where id=upd.x" upVal
        if (result > 0)
            then pure " Successfully executed the query "
        else 
            pure " Failed: No rows updated "
    
           


api :: Proxy API
api = Proxy


app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
-------------------------------------------------ConnectionToDatabase---------------------------------------------------------------

connection :: IO Connection
connection = 
    connect (ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"})



-----------------------------------------------------------Index---------------------------------
type PageAPI =  Get '[HTML] (Html())

pageServer :: Server PageAPI
pageServer = return page


page :: Html()
page = 
    html_
    (do head_
          (do title_ "Index page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background:white}")
        body_
          (do div_ [id_ "header",style_ "color:black"] "Test"
              p_ (span_ (strong_ "Index Page"))
              hr_ []
              a_ [href_ "http://localhost:8080/index/insertCust"] "Insert Form"
              br_ []

              a_ [href_ "http://localhost:8080/index/displayCust"] "Display"
              br_ []

              a_ [href_ "http://localhost:8080/index/updateCust"] "Update Form"
              br_ []

              a_ [href_ "http://localhost:8080/index/deleteCust"] "Delete Form"
              br_ []
             ))




--------------------------------------------------------------insert---------------------------------------------------------

type InsertAPI = Get '[HTML] (Html())

insertServer :: Server InsertAPI
insertServer = return insertQueryUI


insertQueryUI:: Html()
insertQueryUI =
        html_
            (do head_
                    (do title_ "Display"
                        link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                        style_ "body {background:lightblue")
                body_
                    (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                        form_ [id_ "customerForm", method_ "post", action_ "http://localhost:8080/insert"]
                            (do label_ "Customer Id"
                                input_ [type_ "text",name_ "custId"]
                                br_ []
                                
                                label_ "Customer Reference"
                                input_ [type_ "text",name_ "custCustomerRef"]
                                br_ []

                                label_ "Title"
                                input_ [type_ "text",name_ "custTitle"]                                
                                br_ []

                                label_ "Fullname"
                                input_ [type_ "text",name_ "custFullName"]
                                br_ []

                                label_ "Email"
                                input_ [type_ "text",name_ "custEmail"]
                                br_ []

                                label_ "Phone"
                                input_ [type_ "text",name_ "custPhone"]                                
                                br_ []

                                label_ "Client Id"
                                input_ [type_ "text",name_ "custClientId"]                                
                                br_ []

                                label_ "No. of Booking"
                                input_ [type_ "text",name_ "custNumberOfBookings"]                                
                                br_ []

                                label_ "Last booking created at"
                                input_ [type_ "datetime-local",name_ "custLastBookingCreatedAt"]                                
                                br_ []

                                label_ "Last booking created at"
                                input_ [type_ "datetime-local",name_ "lastBooking"]  
                                br_ []

                                label_ "Created at"
                                input_ [type_ "datetime-local",name_ "custCreatedAt"]  
                                br_ []

                                label_ "Updated at"
                                input_ [type_ "datetime-local",name_ "custUpdatedAt"]  
                                br_ []

                                input_ [type_ "submit",name_ "Submit",value_ "Submit"]

                                )     ) ) 

---------------------------------------------------------update---------------------------------------------------------

type UpdateAPI = Get '[HTML] (Html())

updateServer :: Server UpdateAPI
updateServer = return updateQueryUI



updateQueryUI:: Html()
updateQueryUI =
    html_
        (do head_
                (do title_ "Update"
                    link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                    style_ "body {background:lightblue")
            body_
                (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                    form_ [id_ "updateForm", method_ "post", action_ "http://localhost:8080/update"]
                        (do label_ "Customer Id"
                            input_ [type_ "text",name_ "custIdUpdate"]
                            br_ []   

                            label_ "No. of Booking"
                            input_ [type_ "text",name_ "custNumberOfBookingsUpdate"]                                
                            br_ [] 

                            input_ [type_ "submit",name_ "Submit",value_ "Submit"]
                            
                                                            )     ) ) 
 
----------------------------------------------------------------delete---------------------------------------------------

type DeleteAPI = Get '[HTML] (Html())

deleteServer :: Server DeleteAPI
deleteServer = return deleteQueryUI



deleteQueryUI:: Html()
deleteQueryUI =
        html_
            (do head_
                    (do title_ "Delete"
                        link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                        style_ "body {background:lightblue")
                body_
                    (do div_ [id_ "header",style_ "color:white"] "Delete Customer"
                        form_ [id_ "customerForm", method_ "post",action_ "http://localhost:8080/delete"]
                            (do label_ "Customer Id"
                                input_ [type_ "text",name_ "custIdDel"]
                                br_ []

                                input_ [type_ "submit",name_ "Submit",value_ "Submit"]                                
                                                                )     ) ) 


----------------------------------------------------display-------------------------------------------------------------

type DisplayAPI = Get '[HTML] (Html())

displayServer :: Server DisplayAPI
displayServer = customerDisplay 


displayQueryUI :: [(Integer, String)] -> Html()
displayQueryUI queryResult = do
    html_
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

customerDisplay :: Handler (Html())
customerDisplay = do
    conn <- CM.liftIO $ connection  
    xs <- CM.liftIO $  query_ conn "select id,title from customers" 
    pure $ displayQueryUI (xs)




    {-
 customerPg :: Html()
customerPg = CM.liftIO (fmap (\c -> customerPage c) (getCustomers))   
    

getCustomers :: Control.Monad.IO.Class.MonadIO m => m [Customer]
    -}