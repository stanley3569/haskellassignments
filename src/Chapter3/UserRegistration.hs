module Chapter3.UserRegistration where

import Data.List (break,null,filter,take)

data Email = MkEmail String deriving (Eq, Show, Ord)

data User = MkUser
    {
        userEmail :: Email,
        userFullName :: String,
        userPassword :: String,
        userPostalCode :: String,
        userStatus :: String,
        userVerificationCode :: String
    } deriving (Eq, Show, Ord)


data NewUser = MkNewUser
    {
        nuserEmail :: Email,
        nuserFullName :: String,
        nuserPassword :: String,
        nuserPostalCode :: String
    } deriving (Eq, Show, Ord)


registerUser :: NewUser -> [User] -> [User]
registerUser newUser@MkNewUser {nuserEmail = (MkEmail email)} userDb =
    let user = MkUser{
        userEmail = (nuserEmail newUser),
        userFullName = (nuserFullName newUser),
        userPassword = (nuserPassword newUser),
        userPostalCode = (nuserPostalCode newUser),
        userStatus = "unverified",
        userVerificationCode = (take 2 email) ++ ( take 2 (nuserFullName newUser) ) ++ ( take 2 (nuserPostalCode newUser) )
    }
    in (user:userDb)


verifyUser :: Email -> String -> [User] -> (Bool,String,[User])
verifyUser e code userDb = 
    let existingUsers = Data.List.filter (\u -> (userEmail u)==e) userDb
    in if(Data.List.null existingUsers)
        then (False,"No such user",userDb)
       else let existingUser = head existingUsers
            in if(code==(userVerificationCode existingUser))
                then let verfiedUser = existingUser {userStatus="active"}
                         newUserDb = replaceUserInDb verfiedUser userDb
                     in (True,"Verified",newUserDb)
                else (False,"Incorrect verification code",userDb)


deactivateUser :: Email -> [User] -> (Bool,String,[User])
deactivateUser e userDb = 
    let existingUsers =Data.List.filter (\u -> (userEmail u)==e) userDb
    in if(Data.List.null existingUsers)
        then (False,"No such user",userDb)
       else let existingUser = head existingUsers
                deactiveUser = existingUser{userStatus="deactivated"}
            in (True,"User deactivated",replaceUserInDb deactiveUser userDb)


replaceUserInDb :: User -> [User] -> [User]
replaceUserInDb u userDb = 
    let (a,b) = Data.List.break (\x -> (userEmail x) == (userEmail u) ) userDb
    in if(Data.List.null b)
        then (a++[u])
        else (a++(u:(tail b)))
















--let db1=[]
--let db2 = registerUser MkNewUser{ nuserEmail = (MkEmail "mojo@gmail.com"), nuserFullName ="Mojo Jojo",nuserPassword="unsafe",nuserPostalCode="110001"} db1

---[MkUser {userEmail = MkEmail "mojo@gmail.com", userFullName = "Mojo Jojo", userPassword = "unsafe", userPostalCode = "110001", userStatus = "unverified", userVerificationCode = "moMo11"}]



---let db3 = registerUser MkNewUser{ nuserEmail = (MkEmail "dexter@gmail.com"), nuserFullName ="Dexter dede",nuserPassword="unsafeagain",nuserPostalCode="968811"} db2

------[MkUser {userEmail = MkEmail "dexter@gmail.com", userFullName = "Dexter dede", userPassword = "unsafeagain", userPostalCode = "968811", userStatus = "unverified", userVerificationCode = "deDe96"},MkUser {userEmail = MkEmail "mojo@gmail.com", userFullName = "Mojo Jojo", userPassword = "unsafe", userPostalCode = "110001", userStatus = "unverified", userVerificationCode = "moMo11"}]


--let (result,reason,db4) = verifyUser (MkEmail "mojo@gmail.com") "incorrect" db3
--result
        --False
--reason
        --"Incorrect verification code"

--db4
    ----[MkUser {userEmail = MkEmail "dexter@gmail.com", userFullName = "Dexter dede", userPassword = "unsafeagain", userPostalCode = "968811", userStatus = "unverified", userVerificationCode = "deDe96"},MkUser {userEmail = MkEmail "mojo@gmail.com", userFullName = "Mojo Jojo", userPassword = "unsafe", userPostalCode = "110001", userStatus = "unverified", userVerificationCode = "moMo11"}]


--let (result,reason,db4) = verifyUser (MkEmail "mojo@gmail.com") "moMo11" db3
   
--db4
        -- [MkUser {userEmail = MkEmail "dexter@gmail.com", userFullName = "Dexter dede", userPassword = "unsafeagain", userPostalCode = "968811", userStatus = "unverified", userVerificationCode = "deDe96"},MkUser {userEmail = MkEmail "mojo@gmail.com", userFullName = "Mojo Jojo", userPassword = "unsafe", userPostalCode = "110001", userStatus = "active", userVerificationCode = "moMo11"}]





--let (result,reason,db5) = deactivateUser (MkEmail "incorrect@incorrect.com") db4

--result
        ----False
--reason
        ----"No such user"

--let (result,reason,db5) = deactivateUser (MkEmail "dexter@gmail.com") db4
--db5
        ----[MkUser {userEmail = MkEmail "dexter@gmail.com", userFullName = "Dexter dede", userPassword = "unsafeagain", userPostalCode = "968811", userStatus = "deactivated", userVerificationCode = "deDe96"},MkUser {userEmail = MkEmail "mojo@gmail.com", userFullName = "Mojo Jojo", userPassword = "unsafe", userPostalCode = "110001", userStatus = "active", userVerificationCode = "moMo11"}]



{-}
countUsers :: String -> [User] -> Int
countUsers status userDb = length (filter (\u -> (userStatus u) == status) userDb)

userStatusSummary :: [User] -> [(String, Int)]
userStatusSummary userDb =
  --let statuses = ["verified", "inactive", "deactive"]
  let statuses = ["active","deactivated","unverified"]
  in map (\status -> (status, countUsers status userDb)) statuses
-}




{-}
data Status = Active
| Inactive
| Deactive
deriving (Eq, Show, Ord)

data User = MkUser
{ userEmail :: Email
, userFullName :: String

-- NOTE: Bad idea alert!
, userPassword :: String

, userPostalCode :: String
, userStatus :: Status
, userVerificationCode :: String
} deriving (Eq, Show, Ord)

-}





