{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tweet1 where                                                     ----working

import Network.Wreq
import Control.Lens as CL hiding (id)
import Data.Aeson as DA 
import Data.Aeson.Lens (key,nth)
import Data.ByteString.Char8 as DB
import Data.String.Conv
import Data.Map as Map
import Data.Text
import Prelude hiding (id)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.List as DL


authenticate :: Options
authenticate  =  
    let auth1 = oauth1Auth (DB.pack "mqC2gEAkGVxVjZJ9Drn5k08A7") (DB.pack  "oj6gKUWjFwLVo9UVUzvayFvfmf4vVbpu7RV0K4ZUTXAoz8v5GY") (DB.pack "976346669745778688-BIIdmsBNd4BUKkLG3LlCwt1AfOHwWyH") (DB.pack "1Btf70miGr2eg7JsAKmji1QLECDeIhwWNvjwGu869YmX3")
        opts = defaults & header "Accept" .~ ["application/json"]
     in opts & auth ?~ (auth1)

-----------------------------------------
data User = User {
    id :: Integer,
    screen_name :: String
    } deriving (Show,Eq,Generic)


instance FromJSON User where
    parseJSON = withObject "User" $ \v -> do
        id1 <- v .: "id"
        screen_name1 <- v .: "screen_name"
        return $ User {
            id=id1,
            screen_name=screen_name1
        }

data Responses = Responses {
    users :: [User]
    } deriving (Show,Eq,Generic)

instance FromJSON Responses where
    parseJSON (Object v)= do
        users1 <- v .: "users"
        pure (Responses {users=users1} )


-----------------------------------

data Tweets = Tweets {
    text :: String
    } deriving (Show,Eq,Generic)



instance FromJSON Tweets where
    parseJSON (Object v) =  do
        text1 <- v .: "text"
        pure $ Tweets {text=text1}
------------------------------------------


urldata :: String
urldata = "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=stanleyf2619&skip_status=true&include_user_entities=false" 


--gets followers id and screen name
getData :: String -> IO (Either String Responses)
getData urldata =  
         getWith authenticate urldata >>= \body1 -> getBody body1


getBody ::  (Response BL.ByteString) -> IO (Either String Responses)
getBody fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody) )



getIds :: (Either String Responses) -> [(String)]
getIds fList =
   let Right Responses {users = follArr} = fList
   in DL.map (\ User { id = usrid, screen_name=sname} -> (sname) ) follArr

--gives followers screen name ["aatishVL","StanleyFloFer"]
--getIds (Right (Responses {users = [User {id = 976334890915397632, screen_name = "aatishVL"},User {id = 1172617668, screen_name = "StanleyFloFer"}]}))





--extractids :: IO [String]
extractids = 
    let data1 = getData urldata
    in data1 >>= \body1 -> pure $  (allTweets (getIds body1) )
               -- DL.foldl' (\(id,uname) arr-> (getWith newHead "https://api.twitter.com/1.1/search/tweets.json?screen_name=aatishVL") ) []   (getIds body1)




------------------------------------------

--use for recursive
urldata2 :: String
urldata2 = "https://api.twitter.com/1.1/statuses/user_timeline.json?q=blabla&screen_name="


-- use for testing
urldata3 :: String
urldata3 = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=aatishVL"



allTweets :: [String] -> IO (Either String [Tweets])
allTweets listusers = do
    getWith authenticate (urldata2++(DL.head listusers) ) >>= \body1 -> getBody1 body1


--gets tweets of a single user   Right [Tweets {text = "blabla"},Tweets {text = "fsfdf"}] 
getData1 :: String -> IO (Either String [Tweets])
getData1 urldata3 =  
    getWith authenticate urldata3 >>= \body1 -> getBody1 body1


getBody1 ::  (Response BL.ByteString) -> IO (Either String [Tweets])
getBody1 fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody ) )


-- gives a list of tweets
getTweets :: (Either String [Tweets]) -> [String]
getTweets fList =
   let Right xs = fList
   in DL.map (\ Tweets {text = tarr} -> tarr) xs





allTweets1 :: [String] -> IO [Either String [Tweets]]
allTweets1 listusers = do
    --llist <- 
    mapM (\listu -> (getWith authenticate (urldata2++(listu) ) >>= \body1 -> getBody1 body1)  ) listusers

    --llist >>= \llist1 -> mapM (\(Right xs) -> (mapM(\ Tweets {text=ttt} -> ttt) xs)  ) llist1
    -- DL.map (\Tweets {text = tarr} -> tarr) xs





{-
getIds :: (Either String Responses) -> [(String)]
getIds fList =
   let Right Responses {users = follArr} = fList
   in DL.map (\ User { id = usrid, screen_name=sname} -> (sname) ) follArr


-}






{-}
extractids = 
    let data1 = getData urldata
    in data1 >>= \body1 -> pure $ getIds body1
-}



--https://api.twitter.com/1.1/search/tweets.json?q=blabla&screen_name=aatishVL



{-
getIds :: (Either String FollowerList) -> [Integer]
getIds fList =
   let Right FollowerList {followerList = follArr} = fList
   in DL.map (\Follower{
       id=usrid,name=_,screen_name=_,followers_count=_
   } -> usrid) follArr


-}



