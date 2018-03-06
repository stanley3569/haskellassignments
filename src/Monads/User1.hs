module Monads.User1 where

import Data.List --(break, null, filter, take)
import Text.Read
import Data.Char

data Email = MkEmail String deriving (Eq, Show, Ord)


data User = MkUser
  { userEmail :: Email
  , userFullName :: String
  , userPassword :: String

  , userPostalCode :: String
  , userStatus :: String
  , userVerificationCode :: String
  } deriving (Eq, Show, Ord)

data NewUser = MkNewUser
  { nuserEmail :: Email
  , nuserFullName :: String
  , nuserPassword :: String
  , nuserPostalCode :: String
  } deriving (Eq, Show, Ord)

data Status = Active | Inactive | Deactive deriving (Eq, Show, Ord)


registerUser :: NewUser -> [User] -> Either String [User]
registerUser newUser@MkNewUser{nuserEmail=(MkEmail email)} userDb =
  let user = MkUser
              { userEmail = (nuserEmail newUser)
              , userFullName = (nuserFullName newUser)
              , userPassword = (nuserPassword newUser)
              , userPostalCode = (nuserPostalCode newUser)
              , userStatus = "unverified"
              , userVerificationCode = (take 2 email) ++ (take 2 (nuserFullName newUser)) ++ (take 2 (nuserPostalCode newUser))
              }
      existingUser = Data.List.filter (\u -> (userEmail u) == ((\x -> (userEmail x))user) ) userDb
  in if(Data.List.null existingUser )
        then Right (user:userDb)
      else Left "--Username already exists-- Re-enter the value" --userDb

verifyUser :: Email -> String -> [User] -> Either String (Bool, String, [User])
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No user exists in the db"   --(False, "No such user", userDb)
        else  let existingUser = head existingUsers
              in  if (code==(userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{userStatus="active"}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  Right (True, "Verified", newUserDb)
                    else Left "No user exists in the db"   --(False, "Incorrect verification code", userDb)

deactivateUser :: Email -> [User] -> Either String (Bool, String, [User])
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "--User doesnt exist--"         --(False, "No such user", userDb)
        else  let existingUser = head existingUsers
                  deactiveUser = existingUser{userStatus = "deactivated"}
              in  Right (True, "User deactivated", replaceUserInDb deactiveUser userDb)

replaceUserInDb :: User -> [User] -> [User]
replaceUserInDb u userDb =
  let (a, b) = Data.List.break (\x -> (userEmail x)==(userEmail u)) userDb
  in if (Data.List.null b) 
      then  (a ++ [u])
      else  (a ++ (u:(tail b)))

     
countUsers :: String -> [User] -> Int
countUsers status userDb = length (filter (\u -> (userStatus u) == status) userDb)


userStatusSummary :: [User] -> [(String, Int)]
userStatusSummary userDb =
  let statuses = ["unverified", "Active", "deactivated"]
  in map (\status -> (status, countUsers status userDb)) statuses
  

{-}
notEmpty1 :: (Monoid a, Eq a) => a -> Either String a
notEmpty1 x =
              if (x == mempty)
                then Left "is empty"
                else Right x

validatePostal1 :: String -> Bool
validatePostal1 xs =
                  (all isDigit xs) && (length xs ==6)


validateName1 :: String -> Bool
validateName1 xs =
                (all isAlpha xs ) && (length xs >= 5)

passwordValidation1 :: String -> Bool               
passwordValidation1 xs =
                (length xs >=6) && (any isUpper xs)  && (any isLower xs) 
-}






registerUser1 ::[User] -> IO [User]
registerUser1  userDb =
  putStrLn "Enter the email " >>  getLine >>= \email -> validateEmail email >>= \email ->
    putStrLn "Enter the name " >> getLine >>= \name -> validateName name >>= \name ->
      putStrLn "Enter the password " >> getLine >>= \password -> validatePassword password >>= \password ->
        putStrLn "Enter the postal code " >> getLine >>= \postalcode -> validatePostal postalcode >>= \postalcode ->
          let newuser = MkNewUser{nuserEmail = MkEmail email, nuserFullName = name, nuserPassword = password, nuserPostalCode = postalcode}
              output =   (registerUser newuser userDb)
          in case output of
                Left err -> putStrLn err >> registerUser1 userDb
                Right udb -> pure udb


validateEmail :: String -> IO String
validateEmail email =
          let (a1,a2) = (break(=='@') email)
              (b1,b2) = (break(=='.') a2)
              checkd = (".com" `isInfixOf` a2) || (".co.in" `isInfixOf` a2)
          in if(checkd==True)
                then pure email
              else putStrLn "--Invalid Email--Re-enter the email--" >> getLine >>= \email1 -> validateEmail email1
        

validateName :: String -> IO String              
validateName name = 
              if ( (length $ filter (/=' ') name) >= 4   &&  (all isAlpha name ))
                then pure name
              else putStrLn "--Invalid Name Size--Re-enter the name--" >> getLine >>= \name1 -> validateName name1

validatePassword :: String -> IO String         
validatePassword password =
              if ((length password >=6) && (any isUpper password)  && (any isLower password))
                then pure password
              else putStrLn "--Invalid Password Size--Re-enter the Password--" >> getLine >>= \password1 -> validatePassword password1

validatePostal :: String -> IO String             
validatePostal postalcode =
              if((all isDigit postalcode) && (length postalcode ==6))
                then pure postalcode
              else putStrLn "--Invalid Postal Code--Re-enter the Postal code--" >> getLine >>= \password1 -> validatePassword password1

validateVerificationCode :: String -> IO String
validateVerificationCode verificationcode =
              if (length verificationcode /= 0)
                then pure verificationcode
              else putStrLn "--Invalid verification Code--Re-enter the verification code--" >> getLine >>= \verificationcode1 -> validateVerificationCode verificationcode1
               



verifyUser1 :: [User] -> IO (Bool, String, [User])
verifyUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email -> validateEmail email >>= \email ->
    putStrLn "Enter the code " >> getLine >>= \code ->
                let output = (verifyUser (MkEmail email) code userDb)
                in case output of
                      Left err -> putStrLn err >> verifyUser1 userDb
                      Right udb ->pure udb


deactivateUser1 :: [User] -> IO (Bool, String, [User])
deactivateUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email -> validateEmail email >>= \email ->
            let output = (deactivateUser (MkEmail email) userDb)
            in case output of
                    Left err -> putStrLn err >> deactivateUser1 userDb
                    Right udb -> pure udb

replaceUser1 :: [User] -> IO [User]
replaceUser1 userDb =
  putStrLn "Enter the email " >>  getLine >>= \email -> validateEmail email >>= \email ->
    putStrLn "Enter the name " >> getLine >>= \fullname -> validateName fullname >>= \fullname ->
      putStrLn "Enter the password " >> getLine >>= \password -> validatePassword password >>= \password ->
        putStrLn "Enter the postalcode " >> getLine >>= \postalcode -> validatePostal postalcode >>= \postalcode ->
          putStrLn "Enter the status " >> getLine >>= \status ->
            putStrLn "Enter the verification code " >> getLine >>= \verificationcode -> validateVerificationCode verificationcode >>= \verificationcode ->
              let nuser = MkUser{ userEmail = MkEmail email, userFullName = fullname, userPassword = password, userPostalCode = postalcode, userStatus=status,userVerificationCode=verificationcode} 
                in pure (replaceUserInDb nuser userDb)


countUsers1 :: [User] -> IO Int
countUsers1 userDb =
  putStrLn "Enter the status " >> getLine >>= \status ->
                      pure( countUsers status userDb )

{-}
userStatusSummary1 userDb =
        pure(userStatusSummary userDb)
-}


display1 :: (Applicative f, Foldable t) => t User -> f [Char]
display1 userDb =
  let display = foldl' (\arr MkUser{userEmail=MkEmail email,userFullName = name,userPassword = password,userPostalCode = postalcode,userStatus = status,userVerificationCode = verifycode} ->
        arr++" "++email++" "++name++" "++password++" "++postalcode++" "++status++" "++verifycode++"\n") "" userDb
  in pure display



main1 :: [User] -> IO [Char]
main1 userDb = (putStrLn "\n 1. register user \n 2. replace user \n 3. deactivate user \n 4. verify user \n 5. display users \n") >>
                    getLine >>= \ choice ->
                        case (readMaybe choice)::Maybe Int of
                                 Just 1 -> registerUser1 userDb >>= (\(userdb)-> main1 userdb)
                                 Just 2 -> replaceUser1 userDb >>= (\userdb -> main1 userdb)
                                 Just 3 -> deactivateUser1 userDb  >>= (\(_,_,userdb)->main1 userdb )
                                 Just 4 -> verifyUser1 userDb >>= (\(_,_,userdb) -> main1 userdb)
                                 Just 5 -> display1 userDb                               
                                 Just _   -> main1 userDb
                                 Nothing -> main1 userDb







