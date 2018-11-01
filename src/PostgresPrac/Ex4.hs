{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ex4 where

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


data User = User {
    userid :: Int,
    username :: String,
    userValid :: Bool
    } deriving Generic
-- /endpoint/int/:integer/str/:string/bool/:bool


--data User = User (Int,String,Bool) deriving Generic

type UserAPI = "endpoint"
                :> ReqBody '[JSON] User
                :> Post '[JSON] String

instance ToJSON User 


instance FromJSON User 


server :: Server UserAPI
server = user 
            where user :: User -> Handler String
                  user User{userid=x,username=y,userValid=z} = return ((show x)++" "++y++" "++ (show z))

                 
api :: Proxy UserAPI
api = Proxy


app :: Application
app = serve api server

main :: IO ()
main = run 8080 app



    





            --curl -v -H "Content-Type: application/json" -X POST \
 --   -d '{"intVal":123,"stringVal":"abc","boolVal":true}' http://localhost:8081/endpoint



 