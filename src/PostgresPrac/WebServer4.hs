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

module WebServer4 where



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
import qualified Data.Aeson.Parser
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm(..), ToForm(..))
import Servant.Server

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




insertUI ::  Html()
insertUI =
    html_
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


type InsertAPI = "insertCust" 
    :>  Get '[HTML] (Html())

insertServer :: Server InsertAPI 
insertServer =  return insertUI 

-----------------------------------------------------

server :: Server API
server =  pageServer :<|> insertServer

api :: Proxy API
api = Proxy


app :: Application
app = serve api server

main :: IO ()
main = run 8080 app


-------------------------------------------------------------


type API = 
    "index" :> PageAPI 
    :<|> "index" :> InsertAPI

------------------------------------Index--------------------------
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


----------------------------



