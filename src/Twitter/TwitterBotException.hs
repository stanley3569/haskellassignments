{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TwitterBotException where                                                     

import Network.Wreq
import Control.Lens as CL hiding (id)
import Data.Aeson as DA 
--import Data.Aeson.Lens (key)
import Data.ByteString.Char8 as DB
import Data.String.Conv
--import Data.Map as Map
import Data.Text as DT
import Prelude hiding (id)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.List as DL
import Control.Exception
import Control.Monad as CM
--import Data.Aeson.Lens (_String,key)

authenticate :: Options
authenticate  =  
    let auth1 = oauth1Auth (DB.pack "mqC2gEAkGVxVjZJ9Drn5k08A7") (DB.pack  "oj6gKUWjFwLVo9UVUzvayFvfmf4vVbpu7RV0K4ZUTXAoz8v5GY") (DB.pack "976346669745778688-BIIdmsBNd4BUKkLG3LlCwt1AfOHwWyH") (DB.pack "1Btf70miGr2eg7JsAKmji1QLECDeIhwWNvjwGu869YmX3")
        --opts = defaults & header "Accept" .~ ["application/json"]
     in defaults & auth ?~ (auth1)

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
    parseJSON _ = error "Invalid Data"

data Responses = Responses {
        users :: [User]
    } deriving (Show,Eq,Generic)

instance FromJSON Responses where
    parseJSON (Object v)= do
        users1 <- v .: "users"
        pure (Responses {users=users1} )
    parseJSON _ = error "Invalid Data"


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
    parseJSON _ = error "Invalid Data"
--------------------------------------



urldata :: String
urldata = "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=stanleyf2619&skip_status=true&include_user_entities=false" 



getData :: String -> IO [Status]
getData urldata = do
        let follow = getWith authenticate urldata >>= \body1 -> getBody body1
        follow >>= \data1 ->  matchInput (allTweets (getFollowerIds data1))

getBody ::  (Response BL.ByteString) -> IO (Either String Responses)
getBody fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody) )


getFollowerIds :: (Either String Responses) -> [(ScreenName)]
getFollowerIds fList =
   let Right Responses {users = follArr} = fList
   in DL.map (\ User { id = _, screen_name=screenName} -> (screenName) ) follArr

type TweetText = String
type TweetIdStr = String
type ScreenName = String
----------------------------------


followerTimeline :: String
followerTimeline = "https://api.twitter.com/1.1/statuses/user_timeline.json?q=blabla&screen_name="



getBody1 ::  (Response BL.ByteString) -> IO (Either String [Tweets])
getBody1 fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody ) )



allTweets :: [String] ->  IO [(TweetIdStr,TweetText)]
allTweets listusers = do
    let tlist = mapM (\listu -> (getWith authenticate (followerTimeline++(listu) ) >>= \body1 -> getBody1 body1)  ) listusers
    tlist >>= \data1 -> pure (getIdTweet data1)





getIdTweet ::  [Either String [Tweets]] -> [(TweetIdStr,TweetText)]
getIdTweet data1 = 
    Prelude.concat $    
                DL.map (\(Right tlist ) -> (
                    DL.map (\ ( Tweets {id_str=tweetId, text=tweetText} ) -> (tweetId,tweetText)  ) tlist)    ) data1                    



matchInput :: IO [(TweetIdStr,TweetText)] -> IO [Status]
matchInput idTweet = do
    Prelude.putStrLn "Enter the data to search"
    let topicSearch = Prelude.getLine
    postRetweet (topicSearch >>=  \(word1) ->do
                                    idTweet >>= \idTweet1 -> pure $
                                      (DL.foldl' (\arr (x,y) -> if (DT.isInfixOf ( (toS word1)) (toS y) )
                                                                    then arr++[x]
                                                                 else arr   ) []   (idTweet1)     ) )


retweetUrl :: String
retweetUrl = "https://api.twitter.com/1.1/statuses/retweet/"






postRetweet :: IO [TweetIdStr] -> IO [Status]
postRetweet tweetId = do
    tweetId >>= \tweetId1 -> (CM.foldM (\arr tid ->  ((try (postWith authenticate (retweetUrl++tid++".json") (DB.pack "ABC")) ):: IO (Either SomeException (Response BL.ByteString)) ) >>= 
         \sc ->
                case sc of
                    Right val ->   if (val ^. responseStatus ^. statusCode)==200
                                        then pure (arr++[val ^. responseStatus])  
                                    else pure (arr)              
                    Left x -> pure (arr) ) [] tweetId1 )






{-}
--postRetweet1 :: IO [TweetIdStr] -> IO [Status]
postRetweet1 tweetId = do
    tweetId >>= \tweetId1 -> (CM.foldM (\arr tid ->  ((try (postWith authenticate (retweetUrl++tid++".json") (DB.pack "ABC")) ):: IO (Either SomeException (Response BL.ByteString)) ) >>= 
         \sc ->
                case sc of
                    Right val ->   if (val ^. responseStatus ^. statusCode)==200
                                        then pure (arr++[val ^. responseBody . key "text" .])  
                                    else pure (arr)              
                    Left x -> pure (arr) ) [] tweetId1 )
-}


{-}
postRetweet :: IO [TweetIdStr] -> IO [Status]
postRetweet tweetId = do
    let response1 = tweetId >>= \tweetId1 -> mapM (\tid -> (postWith authenticate (retweetUrl++tid++".json") (DB.pack "ABC"))  ) tweetId1
    response1 >>= \rid -> (mapM (\x -> pure (x^. responseStatus) ) rid)
   -} 

{- working
postRetweet :: IO [TweetIdStr] -> IO [Status]
postRetweet tweetId = do
    tweetId >>= \tweetId1 -> (CM.foldM (\arr tid ->  ((try (postWith authenticate (retweetUrl++tid++".json") (DB.pack "ABC")) ):: IO (Either SomeException (Response BL.ByteString)) ) >>= 
         \sc ->
                case sc of
                    Right val ->   if (val ^. responseStatus ^. statusCode)==200
                                        then pure (arr++[val ^. responseStatus])  
                                    else pure (arr)              
                    Left x -> pure (arr) ) [] tweetId1 )



-}
