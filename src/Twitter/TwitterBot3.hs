{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TwitterBot2 where                                                     

import Network.Wreq
import Control.Lens as CL -- hiding (id)
import Data.Aeson as DA 
import Data.ByteString.Char8 as DB
import Data.String.Conv as DSC
import Data.Text as DT
import Prelude hiding (id)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.List as DL
import Control.Exception
import Control.Monad as CM

-----------------------------------------------------------------
authenticate :: Options
authenticate  =  
    let auth1 = oauth1Auth (toS ("mqC2gEAkGVxVjZJ9Drn5k08A7"::String) ) (toS ("oj6gKUWjFwLVo9UVUzvayFvfmf4vVbpu7RV0K4ZUTXAoz8v5GY"::String) ) (toS ("976346669745778688-BIIdmsBNd4BUKkLG3LlCwt1AfOHwWyH"::String) ) (toS ("1Btf70miGr2eg7JsAKmji1QLECDeIhwWNvjwGu869YmX3"::String) )
        opts = defaults
     in opts & auth ?~ (auth1)

-----------------------------------------
type UserIdTwitter = Integer
type ScreenNameTwitter = String

data User = User {
        id :: UserIdTwitter,
        screenName :: ScreenNameTwitter
    } deriving (Show,Eq,Generic)


instance FromJSON User where
    parseJSON = withObject "user" $ \v -> do
        id1 <- v .: "id"
        screenName1 <- v .: "screen_name"
        pure $ User {id=id1, screenName=screenName1}


data Responses = Responses {
        users :: [User]
    } deriving (Show,Eq,Generic)

instance FromJSON Responses where
    parseJSON = withObject "responses" $ \v -> do
        users1 <- v .: "users"
        pure (Responses {users=users1} )



-----------------------------------
type TweetIdStr = String
type TweetText = String

data Tweets = Tweets {
    idStr :: TweetIdStr,
    text :: String
    } deriving (Show,Eq,Generic)



instance FromJSON Tweets where
    parseJSON = withObject "tweets" $ \v ->  do
        idStr1 <- v .: "id_str"
        text1 <- v .: "text"
        pure $ Tweets {idStr=idStr1, text=text1}

--------------------------------------



userFollowers :: String
userFollowers = "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=stanleyf2619&skip_status=true&include_user_entities=false" 



retweetMain :: String -> IO [Status]
retweetMain urldata = do
        let follow = getWith authenticate urldata >>= \body1 -> getBody body1
        follow >>= \data1 ->  pure (getFollowerIds data1) >>= \x -> (allTweets x) >>= \y -> (matchInput y)

getBody ::  (Response BL.ByteString) -> IO (Either String Responses)
getBody fulldata=
    pure (DA.eitherDecode (fulldata^. responseBody) )


getFollowerIds :: (Either String Responses) -> [(ScreenName)]
getFollowerIds fList =
   let Right Responses {users = follArr} = fList
   in DL.map (\ User { id = _, screenName=screenName1} -> (screenName1) ) follArr


--type TweetText = String
--type TweetIdStr = String
type ScreenName = String
----------------------------------


followerTimeline :: String
followerTimeline = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="



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
                    DL.map (\ ( Tweets {idStr=tweetId, text=tweetText} ) -> (tweetId,tweetText)  ) tlist)    ) data1                    



matchInput :: [(TweetIdStr,TweetText)] -> IO [Status]
matchInput idTweet = do
    Prelude.putStrLn "Enter the data to search"
    let topicSearch = Prelude.getLine
    (topicSearch >>=  \(word1) ->
                        postRetweet (DL.foldl' (\arr (x,y) -> if (DT.isInfixOf ( (toS word1)) (toS y) )
                                                                    then arr++[x]
                                                                 else arr   ) []   (idTweet)     ) )


retweetUrl :: String
retweetUrl = "https://api.twitter.com/1.1/statuses/retweet/"



postRetweet :: [TweetIdStr] -> IO [Status]
postRetweet tweetId = do
    (CM.foldM (\arr tid ->  ((try (postWith authenticate (retweetUrl++tid++".json") (("ABC")::BL.ByteString)  ) ):: IO (Either SomeException (Response BL.ByteString)) ) >>= 
         \sc ->
                case sc of
                    Right val ->   if (val ^. responseStatus ^. statusCode)==200
                                        then pure (arr++[val ^. responseStatus])  
                                    else pure (arr)              
                    Left _ -> pure (arr) ) [] tweetId )

