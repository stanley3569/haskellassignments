module MonadTransT1 where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe



greet :: IO ()                               -- type:
greet = do  
            putStr "What is your name? "      -- IO ()
            n <- getLine                      -- IO String
            putStrLn $ "Hello, " ++ n         -- IO ()



{-            
--This will not work because putStr is not in the MaybeT IO monad:
mgreet :: MaybeT IO ()
mgreet = do putStr "What is your name? "
-}

--liftIO :: IO a -> MaybeT IO a
--runMaybeT :: MaybeT IO a -> IO (Maybe a)

mgreet :: MaybeT IO ()                             -- types:
mgreet = do 
            liftIO $ putStr "What is your name? "  -- MaybeT IO ()
            n <- liftIO getLine                    -- MaybeT IO String
            liftIO $ putStrLn $ "Hello, " ++ n     -- MaybeT IO ()





askfor :: String -> IO String
askfor prompt = do
                putStr $ "what is your " ++ prompt ++ "? "
                getLine


survey :: IO (String,String)
survey = do
    n <- askfor "name"
    c <- askfor "favorite color"
    return (n,c)





askfor1 :: String -> IO (Maybe String)
askfor1 prompt = do
    putStr $ "what is your " ++ prompt ++ " (type END to quit)? "
    r <- getLine
    if r == "END"
        then return Nothing
    else return (Just r)


survey1 :: IO (Maybe (String, String))
survey1 = do
   ma <- askfor1 "name"
   case ma of
     Nothing -> return Nothing
     Just n  -> do mc <- askfor1 "favorite color"
                   case mc of
                     Nothing -> return Nothing
                     Just c  -> return (Just (n,c))

                     
--The problem is that survey1 has the familiar staircasing issue which doesn't scale if we add more questions.





askfor2 :: String -> MaybeT IO String
askfor2 prompt = do
   liftIO $ putStr $ "What is your " ++ prompt ++ " (type END to quit)? "
   r <- liftIO getLine
   if r == "END"
     then MaybeT (return Nothing)    -- has type: MaybeT IO String
    else MaybeT (return (Just r))   -- has type: MaybeT IO String

--runMaybeT (askfor2 "stanley")






