module Functors.Backup1 where


import Data.Map.Strict as Map
import Data.List as DL
import Data.Maybe
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




-- [(userId, email)]
userList :: [(Int, String)]
userList =  [
                (2765,"user1@gmail.com"),
                (2766,"user2@gmail.com"),
                (1041,"user3@gmail.com"),
                (1429,"user4@gmail.com"),
                (2393,"user5@gmail.com"),
                (2638,"user6@gmail.com")
                ]
-- [(userId, permissionId, action, class, description)]
individualPermissions :: [(Int, Int, String, String, String)]
individualPermissions = [
                        
                        (1429,44,"edit_trip_confirmation_mode","Trips::Trip","Allowed to edit trip's confirmation mode"),
                        (2393,4,"manage_addons","Trips::Trip","Allowed to edit trip addons"),
                        (2638, 8,"manage_post_trip_email","Trips::Trip","Allowed to edit post trip email"),
                        (2638, 7,"manage_post_trip_email","Trips::Trip","Allowed to edit post trip emailssss")

                        ]

-- [(userId, roleId, roleName)]
roleList :: [(Int, Int, String)]
roleList = [
                (2765,2,"Product manager"),
                (2765,3,"Content manager"),
                (2766,3,"Content manager"),
                (1041,4,"Reservation staff"),
                (1429,5,"Reservation manager"),
                (2393,6,"Finance manager"),
                (2638,9,"Supplier")             
            ]

-- [(userId, roleId, permissionId)]
rolePermissions :: [(Int, Maybe Int, Maybe Int)]
rolePermissions = [
                        (2765,Just 2,Nothing),
                        (2765,Just 3,Nothing),
                        (2766,Just 3,Nothing),
                        (1041,Just 4,Nothing),
                        (1429,Just 5,Just 44),
                        (2393,Just 6,Just 4),
                        (2638,Just 9,Just 8)

                    ]
-- [(userId, roleId, permissionId, action, class, description)]
permissionList :: [(Int, Int, Int, String, String, String)]
permissionList = [
                    (2765,2,2,"manage_calendar","Trips::Trip","xxxxxAllowed to edit Departure calendar"),
                    (2765,3,45,"manage_agent_report","Reports::AgentReport","xxxxAllowed to manage agent reports"),
                    (2766,3,45,"manage_agent_report","Reports::AgentReport","xxxxAllowed to manage agent reports"),
                    (1041,4,46,"manage_agents","Common::Client","xxxxxAllowed to manage agent clients"),         --,
                    (1429,5,44,"edit_trip_confirmation_mode","Trips::Trip","xxxxxAllowed to edit trip's confirm mode"),
                    (2393,6,4,"manage_addons","Trips::Trip","xxxxxxAllowed to edit trip addons"),
                    (2638,9,8,"manage_post_trip_email","Trips::Trip","xxxxxxAllowed to edit post trip email")
                ]



prepareUITable :: UITable
prepareUITable = Map.fromList
                (fmap (\(userid,email)->( User {userId=userid,userEmail=email},( (indiPer userid), (rolePer userid)   ))) userList)
                where
                    indiPer userid= 
                        fmap (\(_,ppid,pact,pclas,pdes)-> 
                            Permission {permissionId=ppid,permissionAction=pact,permissionClass=pclas,permissionDescripton=pdes})
                            (DL.filter (\(puid,_,_,_,_)-> userid == puid) individualPermissions)

                    rolePer userid = 
                        mconcat $ DL.nub $ 
                            fmap (\(_,rid,_)->Map.fromList ( fmap (\ (_,rrid,rrname) -> 
                                                                                        (Role {roleId=rrid,roleName=rrname},
                                                                                                    fmap (\(_,_,ppid1,pact1,pclas1,pdes1)-> 
                                                                                                                    Permission{permissionId=ppid1,permissionAction=pact1,permissionClass=pclas1,permissionDescripton=pdes1})
                                                                                                                                    (DL.filter (\(plUID,plRID,_,_,_,_)-> plUID == userid && plRID == rrid) permissionList)  )
                                                    ) (DL.filter (\(ruid,_,_) -> ruid == userid && (isJust rid)
                                                                                ) roleList)
                                                )) rolePermissions





{-}

display :: UITable -> String
display uiTable = 
    DL.foldl' (\arr (key,value)->arr ++ " | " ++(pad 20 (email key) ) ++ (per value) ) mempty (Map.toList uiTable)
    where
        email key =  case key of  
                        User {userId=_,userEmail=uemail} -> uemail
        per value = 
            case value of 
                (inperm,roles) -> DL.foldl' (\arr1r (Role{roleId=_,roleName=rname},rolperm)->(arr1r++ " | " ++(pad 20 ( rname ) )++ " | " ++
                                        (DL.foldl' (\arr2p Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->(arr2p++ (pad 50 ( des ) ) )) mempty rolperm)
                                    )) mempty (Map.toList roles)++ " | " ++
                     (DL.foldl' (\ppstr Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->ppstr++(pad 50 ( des ) )++ " | ") mempty inperm ) ++ " \n "
-}
                                        
pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')


--showTable :: IO()
--showTable = putStr  (display (prepareUITable))










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
                        where ipermSet = DL.map (\x->x++"\n") iperms

            in zip3 emailDisplay rolNamePer indPer 
            ) (showUITable uiTable)


showTable1:: IO()
showTable1 = putStr (DL.foldl' (\ac x->DL.foldl' (\arr (email,(role,rolePer),iPer)-> arr ++ (email++ " | " ++role++ " | "++pad 40 rolePer++ " | " ++iPer)) ac x) "" (display1 (prepareUITable)))


