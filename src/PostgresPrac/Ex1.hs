{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ex1 where

import Prelude ()
import Prelude.Compat
import Data.Aeson.Types
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS    

data User = User {
    userid :: Integer,
    username :: String,
    userValid :: Bool
    } deriving Generic
-- /endpoint/int/:integer/str/:string/bool/:bool

type UserAPI = "endpoint" :> "int" :> Capture "int" Integer 
        :> "str" :> Capture "str" String 
        :> "bool" :> Capture "bool" Bool
        :> Get '[JSON] User

instance ToJSON User

server :: Server UserAPI
server =  user
            where user :: Integer -> String -> Bool -> Handler User
                  user x y z= return (User x y z)

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
    



