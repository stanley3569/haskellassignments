{-# LANGUAGE TemplateHaskell #-}
module Lens.User1 where

import Data.List 
import Text.Read
import Data.Char
import Control.Lens


data Email = MkEmail String deriving (Eq, Show, Ord) 
data Status = Active | Inactive | Deactive deriving (Eq, Show, Ord)

data User = MkUser
  { _userEmail :: Email
  , _userFullName :: String
  , _userPassword :: String
  , _userPostalCode :: String
  , _userStatus :: Status
  , _userVerificationCode :: String
  } deriving (Eq, Show, Ord)  
makeLenses ''User 
  

data NewUser = MkNewUser
  { _nuserEmail :: Email
  , _nuserFullName :: String
  , _nuserPassword :: String
  , _nuserPostalCode :: String
  } deriving (Eq, Show, Ord)
makeLenses ''NewUser 

registerUser :: NewUser -> [User] -> Either String [User]
registerUser newUser@MkNewUser{_nuserEmail=(MkEmail email)} userDb =
    let user = MkUser
              { _userEmail = (_nuserEmail newUser)
              , _userFullName = (_nuserFullName newUser)
              , _userPassword = (_nuserPassword newUser)
              , _userPostalCode = (_nuserPostalCode newUser)
              , _userStatus = Inactive
              , _userVerificationCode = (take 2 email) ++ (take 2 (_nuserFullName newUser)) ++ (take 2 (_nuserPostalCode newUser))
              }
             
        existingUser = Data.List.filter (\u -> (u^. userEmail) == ((\x -> (x^. userEmail)) user) ) userDb
  in if(Data.List.null existingUser )
        then Right (user:userDb)
      else Left "--Username already exists-- Re-enter the value" --userDb

verifyUser :: Email -> String -> [User] -> Either String [User]
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (u^. userEmail) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No user exists in the db"   --(False, "No such user", userDb)
        else  let existingUser = head existingUsers
              in  if (code==(_userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{_userStatus=Active}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  Right newUserDb
                    else Left "No user exists in the db"   --(False, "Incorrect verification code", userDb)

deactivateUser :: Email -> [User] -> Either String  [User]
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (u^. userEmail) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "--User doesnt exist--"         --(False, "No such user", userDb)
        else  let existingUser = head existingUsers
                  deactiveUser = existingUser{_userStatus = Deactive}
              in  Right (replaceUserInDb deactiveUser userDb)

replaceUserInDb :: User -> [User] -> [User]
replaceUserInDb u userDb =
  let (a, b) = Data.List.break (\x -> (x^. userEmail)==(u^. userEmail)) userDb
  in if (Data.List.null b) 
      then  (a ++ [u])
      else  (a ++ (u:(tail b)))

     
countUsers :: Status -> [User] -> Int
countUsers status userDb = length (filter (\u -> (u^. userStatus) == status) userDb)


userStatusSummary :: [User] -> [(Status, Int)]
userStatusSummary userDb =
  let statuses = [Active, Inactive, Deactive]
  in over mapped (\status -> (status, countUsers status userDb)) statuses
  


----validations----

validateEmail :: String -> Maybe String
validateEmail email =
          let (a1,a2) = (break(=='@') email)
              (b1,b2) = (break(=='.') a2)
              checkd = (".com" `isInfixOf` b2) || (".co.in" `isInfixOf` b2)
          in if ((checkd==True) && (length a1 > 2) && (length b2 >3) && (length b1 >=5))
                then Just email
              else Nothing 
     


validateName :: String -> Maybe String              
validateName name = 
              if ( (length $ filter (/=' ') name) >= 4   &&  (all isAlpha name ))
                then Just name      
              else Nothing       


validatePassword :: String -> Maybe String         
validatePassword password =
              if ((length password >=6) && (any isUpper password)  && (any isLower password) && (length (intersect "&%!@#*" password)>=1) )
                then Just password        
              else Nothing            


validatePostal :: String -> Maybe String             
validatePostal postalcode =
              if((all isDigit postalcode) && (length postalcode ==6))
                then Just postalcode 
              else Nothing 


validateVerificationCode :: String -> Maybe String
validateVerificationCode verificationcode =
              if (length verificationcode /= 0)
                then Just verificationcode     
              else Nothing         



----------

checkEmail :: Maybe String -> IO String
checkEmail xs = 
            case (xs) of
              Just v1 -> pure v1
              Nothing -> (putStrLn "Re-Enter the email " >> getLine >>= \email -> checkEmail(validateEmail email) )

 
checkName :: Maybe String -> IO String
checkName xs = 
            case (xs) of
              Just v1 -> pure v1
              Nothing -> (putStrLn "Re-Enter the name " >> getLine >>= \name -> checkName(validateName name) )
        
checkPassword :: Maybe String -> IO String
checkPassword xs = 
            case (xs) of
              Just v1 -> pure v1
              Nothing -> (putStrLn "Re-Enter the password " >> getLine >>= \password -> checkPassword(validatePassword password) )

checkPostal :: Maybe String -> IO String
checkPostal xs = 
            case (xs) of
              Just v1 -> pure v1
              Nothing -> (putStrLn "Re-Enter the postal code " >> getLine >>= \postalcode -> checkPostal(validatePostal postalcode) )
                      
checkVerification :: Maybe String -> IO String
checkVerification xs = 
            case (xs) of
              Just v1 -> pure v1
              Nothing -> (putStrLn "Re-Enter the verification code " >> getLine >>= \code -> checkPostal(validateVerificationCode code) )
                      
---------------

{-}
registerUser1 ::[User] -> IO [User]
registerUser1  userDb =
  putStrLn "Enter the email " >>  getLine >>= \email -> (checkEmail (validateEmail email) )>>= \email1 ->
    putStrLn "Enter the name " >> getLine >>= \name -> (checkName (validateName name)) >>= \name1 ->
      putStrLn "Enter the password " >> getLine >>= \password -> (checkPassword (validatePassword password)) >>= \password1 ->
        putStrLn "Enter the postal code " >> getLine >>= \postalcode -> (checkPostal(validatePostal postalcode)) >>= \postalcode1 ->
          let newuser = MkNewUser{nuserEmail = MkEmail email1, nuserFullName = name1, nuserPassword = password1, nuserPostalCode = postalcode1}
              output =   (registerUser newuser userDb)
          in case output of
                Left err -> putStrLn err >> registerUser1 userDb
                Right udb -> pure udb
-}
registerUser1 ::[User] -> IO [User]
registerUser1  userDb = do
      putStrLn "Enter the email "
      email <- getLine
      email1 <- (checkEmail (validateEmail email) )
      putStrLn "Enter the name " 
      name <- getLine
      name1 <- (checkName (validateName name))
      putStrLn "Enter the password " 
      password <- getLine
      password1 <- (checkPassword (validatePassword password))
      putStrLn "Enter the postal code "
      postalcode <- getLine
      postalcode1 <-  (checkPostal(validatePostal postalcode))
      let newuser = MkNewUser{_nuserEmail = MkEmail email1, _nuserFullName = name1, _nuserPassword = password1, _nuserPostalCode = postalcode1}
          output =   (registerUser newuser userDb)
      case output of
                  Left err -> putStrLn err >> registerUser1 userDb
                  Right udb -> pure udb








{-}
verifyUser1 :: [User] -> IO [User]
verifyUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email -> (checkEmail (validateEmail email) ) >>= \email1 ->
    putStrLn "Enter the code " >> getLine >>=  \code -> (checkVerification(validateVerificationCode code)) >>= \code1 ->
                let output = (verifyUser (MkEmail email1) code1 userDb)
                in case output of
                      Left err -> putStrLn err >> verifyUser1 userDb
                      Right udb ->pure udb
-}
verifyUser1 :: [User] -> IO [User]
verifyUser1 userDb = do
      putStrLn "Enter the email "
      email <- getLine
      email1 <- (checkEmail (validateEmail email) )
      putStrLn "Enter the code " 
      code <- getLine
      code1 <- (checkVerification(validateVerificationCode code))
      let output = (verifyUser (MkEmail email1) code1 userDb)
      case output of
            Left err -> putStrLn err >> verifyUser1 userDb
            Right udb ->pure udb

{-}
deactivateUser1 :: [User] -> IO [User]
deactivateUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email -> (checkEmail (validateEmail email) )>>= \email1 ->
            let output = (deactivateUser (MkEmail email1) userDb)
            in case output of
                    Left err -> putStrLn err >> deactivateUser1 userDb
                    Right udb -> pure udb
-}

deactivateUser1 :: [User] -> IO [User]
deactivateUser1 userDb = do
          putStrLn "Enter the email "
          email <- getLine
          email1 <-  checkEmail (validateEmail email) 
          let output = (deactivateUser (MkEmail email1) userDb)
          case output of
                  Left err -> putStrLn err >> deactivateUser1 userDb
                  Right udb -> pure udb          





{-}
replaceUser1 :: [User] -> IO [User]
replaceUser1 userDb =
  putStrLn "Enter the email " >>  getLine >>= \email -> (checkEmail (validateEmail email) ) >>= \email1 ->
    putStrLn "Enter the name " >> getLine >>= \fullname ->  (checkName (validateName fullname)) >>= \fullname1 ->
      putStrLn "Enter the password " >> getLine >>= \password -> (checkPassword (validatePassword password)) >>= \password1 ->
        putStrLn "Enter the postalcode " >> getLine >>= \postalcode -> (checkPostal(validatePostal postalcode)) >>= \postalcode1 ->
              let nuser = MkUser{ userEmail = MkEmail email1, userFullName = fullname1, userPassword = password1, userPostalCode = postalcode1} 
                in pure (replaceUserInDb nuser userDb)
-}

replaceUser1 :: [User] -> IO [User]
replaceUser1 userDb = do
        putStrLn "Enter the email "
        email <- getLine
        email1 <- (checkEmail (validateEmail email) )
        putStrLn "Enter the name "
        fullname <- getLine
        fullname1 <-  (checkName (validateName fullname))
        putStrLn "Enter the password "
        password <- getLine
        password1 <- (checkPassword (validatePassword password))
        putStrLn "Enter the postalcode "
        postalcode <- getLine
        postalcode1 <- (checkPostal(validatePostal postalcode)) 
        let nuser = MkUser{_userEmail = MkEmail email1,_userFullName = fullname1,_userPassword = password1,_userPostalCode = postalcode1} 
        pure (replaceUserInDb nuser userDb)


countUser1 :: Applicative f => Status -> [User] -> f String
countUser1 status usrDb = 
            case status of
                    Active   -> pure (show(countUsers Active usrDb))
                    Inactive -> pure (show(countUsers Deactive usrDb))
                    Deactive -> pure (show(countUsers Inactive usrDb))





display1 :: [User] -> IO()
display1 userDb =
  let display = foldl' (\arr MkUser{_userEmail=MkEmail email,_userFullName = name,_userPassword = password,_userPostalCode = postalcode,_userStatus = status,_userVerificationCode = verifycode} ->
        arr++" "++pad 20 email++" | "++pad 20 name++" | "++pad 20 password++" | "++pad 20 postalcode++" | "++pad 20 (show status)++" | "++pad 20 verifycode++ " | " ++ "\n") [] userDb
  in putStrLn display


pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')




{-}
main1 :: [User] -> IO()
main1 userDb = (putStrLn "\n 1. register user \n 2. replace user \n 3. deactivate user \n 4. verify user \n 5. display users \n") >>
                    getLine >>= \ choice ->
                        case (readMaybe choice)::Maybe Int of
                                 Just 1 -> registerUser1 userDb >>= (\(userdb)-> main1 userdb)
                                 Just 2 -> replaceUser1 userDb >>= (\userdb -> main1 userdb)
                                 Just 3 -> deactivateUser1 userDb  >>=  (\(userdb)->main1 userdb )
                                 Just 4 -> verifyUser1 userDb >>= (\(userdb) -> main1 userdb)
                                 Just 5 -> display1 userDb                          
                                 _   -> main1 userDb
                                 
-}

 
main1 :: [User] -> IO()
main1 userDb = do
      putStrLn "\n 1. register user \n 2. replace user \n 3. deactivate user \n 4. verify user \n 5. display users \n"
      choice <- getLine
      case (readMaybe choice)::Maybe Int of
        Just 1 -> registerUser1 userDb >>= (\(userdb)-> main1 userdb)
        Just 2 -> replaceUser1 userDb >>= (\userdb -> main1 userdb)
        Just 3 -> deactivateUser1 userDb  >>=  (\(userdb)->main1 userdb )
        Just 4 -> verifyUser1 userDb >>= (\(userdb) -> main1 userdb)
        Just 5 -> display1 userDb                          
        _   -> main1 userDb
