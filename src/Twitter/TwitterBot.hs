{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tweet4 where                                                     

import Network.Wreq
import Control.Lens as CL hiding (id)
import Data.Aeson as DA 
import Data.Aeson.Lens (key,nth)
import Data.ByteString.Char8 as DB
import Data.String.Conv
import Data.Map as Map
import Data.Text as DT
import Prelude hiding (id)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.List as DL
import Data.Text.IO as DI

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
    parseJSON (Object v)=do
        id1 <- v .: "id"
        screen_name1 <- v .: "screen_name"
        pure $ User {id=id1, screen_name=screen_name1}
    parseJSON _ = pure $ User {}

data Responses = Responses {
        users :: [User]
    } deriving (Show,Eq,Generic)

instance FromJSON Responses where
    parseJSON (Object v)= do
        users1 <- v .: "users"
        pure (Responses {users=users1} )
    parseJSON _ = pure (Responses {})


-----------------------------------

data Tweets = Tweets {
    id_str :: String,
    text :: String
    } deriving (Show,Eq,Generic)



instance FromJSON Tweets where
    parseJSON (Object v) =  do
        id_str1 <- v .: "id_str"
        text1 <- v .: "text"
        pure $ Tweets {id_str=id_str1, text=text1}
    parseJSON _ = pure $ Tweets {}
--------------------------------------



urldata :: String
urldata = "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=stanleyf2619&skip_status=true&include_user_entities=false" 






getData :: String -> IO [Response BL.ByteString]
getData urldata = do
        let follow = getWith authenticate urldata >>= \body1 -> getBody body1
        follow >>= \data1 ->  main1 (allTweets1 (getIds data1))

getBody ::  (Response BL.ByteString) -> IO (Either String Responses)
getBody fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody) )



getIds :: (Either String Responses) -> [(String)]
getIds fList =
   let Right Responses {users = follArr} = fList
   in DL.map (\ User { id = usrid, screen_name=sname} -> (sname) ) follArr


----------------------------------


urldata2 :: String
urldata2 = "https://api.twitter.com/1.1/statuses/user_timeline.json?q=blabla&screen_name="

urldata3 :: String
urldata3 = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=aatishVL"






getBody1 ::  (Response BL.ByteString) -> IO (Either String [Tweets])
getBody1 fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody ) )





--gives (tweetid,tweet) for a user
allTweets1 :: [String] ->  IO [(String,String)]
allTweets1 listusers = do
    let tlist = mapM (\listu -> (getWith authenticate (urldata2++(listu) ) >>= \body1 -> getBody1 body1)  ) listusers
    tlist >>= \data1 -> pure (getIdTweet data1)





getIdTweet ::  [Either String [Tweets]] -> [(String,String)]
getIdTweet data1 = 
    Prelude.concat $    
                DL.map (\(Right tlist ) -> (
                    DL.map (\ ( Tweets {id_str=idx,text=text1} ) -> (idx,text1)  ) tlist)    ) data1                    



main1 :: IO [(String,String)] -> IO [Response BL.ByteString]
main1 idTweet = do
    Prelude.putStrLn "Enter the data to search"
    let topicSearch = Prelude.getLine
    postRetweet (topicSearch >>=  \(word1) ->do
                                    idTweet >>= \idTweet1 -> pure $
                                      (DL.foldl' (\arr (x,y) -> if (DT.isInfixOf ( (toS word1)) (toS y) )
                                                                    then arr++[x]
                                                                 else arr   ) []   (idTweet1)     ) )


posturl :: String
posturl = "https://api.twitter.com/1.1/statuses/retweet/"

postRetweet :: IO [String] -> IO [Response BL.ByteString]
postRetweet tweetId =
    tweetId >>= \tweetId1 -> mapM (\tid -> postWith authenticate (posturl++tid++".json") (DB.pack "ABC") ) tweetId1

      