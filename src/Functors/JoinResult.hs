module Functors.JoinResults where   

import Data.Map.Strict as Map
import Data.List as DL

data Permission = Permission
    { permissionId :: Int
    , permissionAction :: String
    , permissionClass :: String
    , permissionDescripton :: String
    } deriving (Eq, Show, Ord)

data Role = Role
    { roleId :: Int
    , roleName :: String
    } deriving (Eq, Show, Ord)               

data User = User
    { userId :: Int
    , userEmail :: String
    } deriving (Eq, Show, Ord)

-- The final type that will be passed to the UI, to display the table.
type UITable = Map.Map User ([Permission], Map.Map Role [Permission])


-- [(userId, email, permissionId, action, class, description, roleId, roleName, permissionId, action, class, description)]
joinResults :: [(Int, String, Maybe Int, Maybe String, Maybe String, Maybe String, Maybe Int, Maybe String, Maybe Int, Maybe String, Maybe String, Maybe String)]
joinResults = [(1,"user1@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
                (1,"user1@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
                (2,"user22222222@bc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
                (1,"user1@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
                (3,"user3@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
                (3,"user3@abc.com",Nothing,Nothing,Nothing,Nothing,Just 1,Just "Reservation Manager",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar"),
                (3,"user3@abc.com",Nothing,Nothing,Nothing,Nothing,Just 2,Just "Reservation Manager",Just 2,Just "manage_Bus",Just "Trips::Trip",Just "Allowed to edit Bus")]






prepareUITable1 = DL.foldl' accumulateMap mempty joinResults
            where
                accumulateMap::UITable -> (Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String)
                                    -> UITable
                accumulateMap uitable (uid, email, Just iPId, Just iPAction, Just iPClass, Just iPDes, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) =
                    let key = User { userId=uid, userEmail=email}
                        val = ([Permission{permissionId=iPId,permissionAction=iPAction, permissionClass=iPClass,permissionDescripton=iPDes}], mempty)
                        in Map.insertWith mergeUser key val uitable
                
                accumulateMap uitable (uid, email, Nothing, Nothing, Nothing, Nothing, Just rid, Just rname, Just rPId, Just rPAction, Just rPClass, Just rPDes) =
                    let key = User{userId=uid, userEmail=email}
                        val = (mempty, singleton  Role{roleId=rid, roleName=rname}  [Permission{permissionId=rPId, permissionAction=rPAction, permissionClass=rPClass, permissionDescripton=rPDes}]) 
                        in Map.insertWith mergeUser key val uitable
                
                accumulateMap _ x = error $ "Invalid data received: " ++ (show x)


                mergeUser :: ([Permission], Map.Map Role [Permission]) -> ([Permission], Map.Map Role [Permission]) -> ([Permission], Map.Map Role [Permission])
                mergeUser (oldPerm, oldRoles) (newPerm, newRoles) = 
                    let finalPerm = DL.union oldPerm newPerm
                        finalRole = Map.unionWith (\oldPerm_ newPerm_ -> DL.union oldPerm_ newPerm_ ) oldRoles newRoles
                    in (finalPerm,finalRole)



showUITable :: UITable -> [(String, ([String], [(String, [String])]))]
showUITable uiTable = DL.map (\(key,value)->( (email key) ,(per value)
                        )) (Map.toList uiTable)
                    where 
                        email key =  case key of  
                                        User {userId=_,userEmail=uemail} -> uemail
                        per value =
                                case value of
                                    (indPer,rolPer) -> ( (fmap (\ Permission {permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->(des)) indPer),
                                                    fmap (\(Role{roleId=_,roleName=rname},rper)->((rname),
                                                                                                    (fmap (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->(des)) rper)
                                                    )) (Map.toList rolPer) )


pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')


display1 :: UITable -> [[(String, (String, String), String)]]
display1 uiTable = 
    DL.map (\ (email,y)-> 
        let
            (iperms,rolep) = y
            maxI= DL.foldl' (\c _ -> c+1) 0 iperms
            maxR= DL.foldl' (\c (_,perm) -> c+(DL.foldl' (\cr _ -> cr+1) 0 perm) ) 0 rolep
            maxOfIR = max maxI  maxR 

            emailDisplay = [pad 20 email ] ++ emailSet
                            where emailSet=(DL.map (\ _ -> (pad 20 "")) [1..maxOfIR-1])

            rolNam = DL.foldl' (\roleL (role,rperms)-> roleL ++ roleSet role rperms ) [] rolep
                     where roleSet role rperms =  zip [(pad 20 role )] rperms               
                                               
            rolNamePer = if (maxR==maxOfIR)
                            then rolNam
                         else rolNam ++ DL.map (\_-> ((pad 20 "" ),(pad 20 "" ))) [1..(maxI-maxR)]

            indPer = if (maxI==maxOfIR)
                            then ipermSet
                        else ipermSet ++ (DL.map (\_-> "\n") [1..(maxR-maxI)])
                        where ipermSet = DL.map (\x->x ++"\n") iperms

            in zip3 emailDisplay rolNamePer indPer 
            ) (showUITable uiTable)


showTable1:: IO()
showTable1 = putStr (DL.foldl' (\ac x->DL.foldl' (\arr (email,(role,rolePer),iPer)-> arr ++ ( email++ " | " ++role++ " | "++pad 40 rolePer++ " | " ++iPer)) ac x) "" (display1 (prepareUITable1)))



