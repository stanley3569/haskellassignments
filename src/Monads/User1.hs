module Monads.User1 where

import Data.List --(break, null, filter, take)

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

registerUser :: NewUser -> [User] -> [User]
registerUser newUser@MkNewUser{nuserEmail=(MkEmail email)} userDb =
  let user = MkUser
              { userEmail = (nuserEmail newUser)
              , userFullName = (nuserFullName newUser)
              , userPassword = (nuserPassword newUser)
              , userPostalCode = (nuserPostalCode newUser)
              , userStatus = "unverified"
              , userVerificationCode = (take 2 email) ++ (take 2 (nuserFullName newUser)) ++ (take 2 (nuserPostalCode newUser))
              }
  in (user:userDb)

verifyUser :: Email -> String -> [User] -> (Bool, String, [User])
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then (False, "No such user", userDb)
        else  let existingUser = head existingUsers
              in  if (code==(userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{userStatus="active"}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  (True, "Verified", newUserDb)
                    else (False, "Incorrect verification code", userDb)

deactivateUser :: Email -> [User] -> (Bool, String, [User])
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then (False, "No such user", userDb)
        else  let existingUser = head existingUsers
                  deactiveUser = existingUser{userStatus = "deactivated"}
              in  (True, "User deactivated", replaceUserInDb deactiveUser userDb)

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
  let statuses = ["unverified", "active", "deactivated"]
  in map (\status -> (status, countUsers status userDb)) statuses
      






registerUser1 :: [User] -> IO [User]
registerUser1  userDb =
  putStrLn "Enter the email " >> getLine >>= \email ->
    putStrLn "Enter the name " >> getLine >>= \name ->
      putStrLn "Enter the password " >> getLine >>= \password ->
        putStrLn "Enter the postal code " >> getLine >>= \postalcode -> 
          let newuser = MkNewUser{nuserEmail = MkEmail email, nuserFullName = name, nuserPassword = password, nuserPostalCode = postalcode} 
          in pure (registerUser newuser userDb)

verifyUser1 :: [User] -> IO (Bool, String, [User])
verifyUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email ->
    putStrLn "Enter the code " >> getLine >>= \code -> 
                            pure (verifyUser (MkEmail email) code userDb)


deactivateUser1 :: [User] -> IO (Bool, String, [User])
deactivateUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email ->
         (   pure  (deactivateUser (MkEmail email) userDb) )    

replaceUser1 :: [User] -> IO [User]
replaceUser1 userDb =
  putStrLn "Enter the email " >> getLine >>= \email ->
    putStrLn "Enter the name " >> getLine >>= \fullname ->
      putStrLn "Enter the password " >> getLine >>= \password ->
        putStrLn "Enter the postalcode " >> getLine >>= \postalcode ->
          putStrLn "Enter the status " >> getLine >>= \status ->
            putStrLn "Enter the verification code " >> getLine >>= \verificationcode ->
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

--display1 :: [User] -> IO()
display1 :: (Applicative f, Foldable t) => t User -> f [Char]
display1 userDb =
  let display = foldl' (\arr MkUser{userEmail=MkEmail email,userFullName = name,userPassword = password,userPostalCode = postalcode,userStatus = status,userVerificationCode = verifycode} ->
        arr++" "++email++" "++name++" "++password++" "++postalcode++" "++status++" "++verifycode++"\n") "" userDb
  in pure display

--main1 :: [User] -> IO [User]


main1 :: [User] -> IO [Char]
main1 userDb = (putStrLn "\n 1. register user \n 2. replace user \n 3. deactivate user \n 4. verify user \n 5. display users \n") >>
                    getLine >>= \ choice ->
                        case choice of
                                  "1" -> registerUser1 userDb >>= (\(userdb)-> main1 userdb)
                                  "2" -> replaceUser1 userDb >>= (\userdb -> main1 userdb)
                                  "3" -> deactivateUser1 userDb  >>= (\(_,_,userdb)->main1 userdb )
                                  "4" -> verifyUser1 userDb >>= (\(_,_,userdb) -> main1 userdb)
                                  "5" -> display1 userDb 
                                  
                              
                                  _   -> main1 userDb







