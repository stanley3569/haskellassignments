{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ex8 where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
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
import Data.Aeson
import Web.FormUrlEncoded
import Servant.HTML.Lucid

{-}
data User = User {
    userid :: Int,
    username :: String,
    userValid :: Bool
    } deriving Generic
-- /endpoint/int/:integer/str/:string/bool/:bool
-}


--------change the HTTP API to accept form-data and output the same string format `<int>, <str>, <bool>` 

data User = User (Int,String,Bool) deriving Generic


type UserAPI =  "endpoint" :> PageAPI
                :<|> "endpoint" :> ReqBody '[FormUrlEncoded] User
                :> Post '[HTML] (Html())


instance ToJSON User 

instance FromJSON User



instance FromForm User where
    fromForm v = do 
        uid <- (parseUnique "userid" v)
        uname <- (parseUnique "username" v)
        uvalid <- (parseUnique "userValid" v)
        pure (User(uid,uname,uvalid))
    





server :: Server UserAPI
server = pageServer :<|> user
            where user :: User -> Handler (Html())
                  user (User(x,y,z) )= return (
                    html_
                        (do head_
                                (do title_ "Index page."
                                    link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                                    style_ "body{background:white}")
                            body_
                                (do div_ [id_ "header",style_ "color:black"] "Test"
                                    (toHtml (show x)) 
                                    (toHtml y) 
                                    (toHtml (show z) )
                                )))




                 
api :: Proxy UserAPI
api = Proxy


app :: Application
app = serve api server

main :: IO ()
main = run 8080 app




type PageAPI = Get '[HTML] (Html())

pageServer :: Server PageAPI
pageServer = return formUI


formUI :: Html()
formUI =
    html_
            (do head_
                    (do title_ "Display"
                        link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                        style_ "body {background:lightblue")
                body_
                    (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                        form_ [id_ "customerForm", method_ "post", action_ "endpoint"]
                            (do label_ "userid"
                                input_ [type_ "text",name_ "userid"]
                                br_ [class_ "formBreak"]
                                
                                label_ "username"
                                input_ [type_ "text",name_ "username"]
                                br_ [class_ "formBreak"]

                                label_ "uservalid"
                                input_ [type_ "text",name_ "userValid"]                                
                                br_ [class_ "formBreak"]

                                input_ [type_ "submit",name_ "Submit",value_ "Submit"]

                                )     ) ) 



dispalyUI :: Integer -> String -> Bool -> Html()
dispalyUI x y z = 
    html_
    (do head_
          (do title_ "Index page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background:white}")
        body_
            (do div_ [id_ "header",style_ "color:black"] "Test"
                (toHtml (show x)) 
                (toHtml y) 
                (toHtml (show z) )
            ))
    

