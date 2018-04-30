{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ex2 where

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
import           Web.FormUrlEncoded          (FromForm)

{-}
data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS    






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
    -}

------------
{-}
type UserAPI1 = "endpoint" :> "int" :> Capture "int" Integer 
        :> "str" :> Capture "str" String 
        :> "bool" :> Capture "bool" Bool
        :> Get '[JSON, HTMLLucid] User


server1 :: Server UserAPI1
server1 =  user
            where user :: Integer -> String -> Bool -> Handler User
                  user x y z =  page x y z

api1 :: Proxy UserAPI1
api1 = Proxy


app1 :: Application
app1 = serve api server1

main1 :: IO ()
main1 = run 8080 app
-}


-------http://localhost:8080/endpoint/int/1/str/hey/bool/True


{-}
data User1 = User1 {
    userid1 :: Integer,
    username1 :: String,
    userValid1 :: Bool
    } deriving (Show,Generic)


instance FromJSON User

type UserAPI1 =  "endpoint" 
            :> "int" :> Capture "int" Integer 
            :> "str" :> Capture "str" String 
            :> "bool" :> Capture "bool" Bool
            :> Post '[JSON] User1

server1 :: Server UserAPI1
server1 =  


api1 :: Proxy UserAPI1
api1 = Proxy


app1 :: Application
app1 = serve api1 server1

main1 :: IO ()
main1 = run 8080 app1
-}


{-}
data User = User {
    userid :: Integer,
    username :: String,
    userValid :: Bool
    } deriving (Eq,Show)
-- /endpoint/int/:integer/str/:string/bool/:bool


type API = "login" :> ReqBody '[FormUrlEncoded] User :> Post '[JSON] Login

data Login = LoggedIn | NotLoggedIn
  deriving (Eq, Show)

instance ToJSON Login where
  toJSON = toJSON . show


instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs =
    User <$> lkp "userid" <*> lkp "username" <*> lkp "userValid"

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> Left $ "label " <> input_label <> " not found"
                 Just v    -> Right v

server :: Server API
server usr =
  if userid usr == 1 && username usr == "sam" && userValid usr == "True"
  then return LoggedIn
  else return NotLoggedIn
-}



-------http://localhost:8080/endpoint/int/1/str/hey/bool/True
instance FromJSON User2

instance ToJSON User2


data User1 = User1 {
    userid1 :: Integer,
    username1 :: String,
    userValid1 :: Bool
    } deriving (Show,Generic)





data User2 = User2 {
    userid2 :: Integer,
    username2 :: String,
    userValid2 :: Bool
    } deriving (Generic)




type UserAPI1 =  "endpoint" 
            :> "int" :> Capture "int" Integer 
            :> "str" :> Capture "str" String 
            :> "bool" :> Capture "bool" Bool
        --    :> Get '[JSON] User2
            :> ReqBody '[JSON] User2
            :> PostNoContent '[JSON] NoContent

-- Server UserAPI1 = Integer -> String -> Bool -> Handler User2

server1 :: Server UserAPI1
server1  =  getUser
                where getUser :: Integer -> String -> Bool -> Handler User2
                      getUser = error "error"
    
    
    --(Integer -> String -> Bool -> Handler User2)
     --   :<|> (Integer -> String -> Bool -> Handler NoContent)

api1 :: Proxy UserAPI1
api1 = Proxy


app1 :: Application
app1 = serve api1 server1

main1 :: IO ()
main1 = run 8080 app1



