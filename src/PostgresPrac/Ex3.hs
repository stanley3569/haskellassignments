{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ex3 where

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

{-}
data User = User {
    userid :: Int,
    username :: String,
    userValid :: Bool
    } deriving Generic
-- /endpoint/int/:integer/str/:string/bool/:bool
-}

data User = User (Int,String,Bool) deriving Generic

type UserAPI = "endpoint"
                :> ReqBody '[JSON] User
                :> Post '[JSON] User

instance ToJSON User 

--instance FromJSON User


instance FromJSON User where
    parseJSON (Object v) = do
        uid <- v .:"userid"
        uname <- v.:"username"
        uvalid <- v.: "userValid"
        pure (User (uid,uname, uvalid))







server :: Server UserAPI
server = user 
            where user :: User -> Handler User
                  user x = return x




                 
api :: Proxy UserAPI
api = Proxy


app :: Application
app = serve api server

main :: IO ()
main = run 8080 app




page :: Integer -> String -> Bool -> Html()
page x y z= 
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
    





            --curl -v -H "Content-Type: application/json" -X POST \
 --   -d '{"intVal":123,"stringVal":"abc","boolVal":true}' http://localhost:8081/endpoint