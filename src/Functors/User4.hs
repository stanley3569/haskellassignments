module Functors.User2 where


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




-- [(userId, email)]
userList :: [(Int, String)]
userList =  [
                (2765,"wolfgang@vacationlabs.com"),
                (2766,"rajdeep+agent@vacationlabs.com"),
                (1041,"rajdeepm@vacationlabs.com"),
                (1429,"supplier-5-content@vacationlabs.com"),
                (2393,"aaditya@vacationlabs.com"),
                (2638,"shubham.k@vacationlabs.com")
                ]
-- [(userId, permissionId, action, class, description)]
individualPermissions :: [(Int, Int, String, String, String)]
individualPermissions = [
                        
                        (1429,44,"edit_trip_confirmation_mode","Trips::Trip","Allowed to edit trip's confirmation mode"),
                        (2393,4,"manage_addons","Trips::Trip","Allowed to edit trip addons"),
                        (2638, 8,"manage_post_trip_email","Trips::Trip","Allowed to edit post trip email")

                        ]

-- [(userId, roleId, roleName)]
roleList :: [(Int, Int, String)]
roleList = [
                (2765,2,"Product manager"),
                (2766,3,"Content manager"),
                (1041,4,"Reservation staff"),
                (1429,5,"Reservation manager"),
                (2393,6,"Finance manager"),
                (2638,9,"Supplier")             --,
                --(2776,6,"Finance manager"),
                --(2775,10,"Marketplace supplier"),
                --(2777,1,"Basic user"),
                --(2778,4,"Reservation staff"),
                --(2765,5,"Reservation manager")

            ]

-- [(userId, roleId, permissionId)]
rolePermissions :: [(Int, Maybe Int, Maybe Int)]
rolePermissions = [
                        (2765,Just 2,Nothing),
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
                    (2766,3,45,"manage_agent_report","Reports::AgentReport","xxxxAllowed to manage agent reports"),
                    (1041,4,46,"manage_agents","Common::Client","xxxxxAllowed to manage agent clients"),         --,
                    (1429,5,44,"edit_trip_confirmation_mode","Trips::Trip","xxxxxAllowed to edit trip's confirmation mode"),
                    (2393,6,4,"manage_addons","Trips::Trip","xxxxxxAllowed to edit trip addons"),
                    (2638,9,8,"manage_post_trip_email","Trips::Trip","xxxxxxAllowed to edit post trip email")
                ]



prepareUITable :: UITable
prepareUITable = Map.fromList
                (fmap (\(userid,email)->( User {userId=userid,userEmail=email},( (t2 userid), (t3 userid)   ))) userList)
                where
                    t2 userid= fmap (\(_,ppid,pact,pclas,pdes)-> Permission {permissionId=ppid,permissionAction=pact,permissionClass=pclas,permissionDescripton=pdes}
                        ) (DL.filter (\(puid,_,_,_,_)-> userid == puid) individualPermissions)

                    t3 userid = mconcat $ DL.nub $ fmap (\(_,rid,_)->
                                    Map.fromList ( 
                                        fmap (\ (_,rrid,rrname) -> 
                                                        (Role {roleId=rrid,roleName=rrname},
                                                            fmap (\(_,_,ppid1,pact1,pclas1,pdes1)-> 
                                                                        Permission{permissionId=ppid1,permissionAction=pact1,permissionClass=pclas1,permissionDescripton=pdes1})
                                                                                        (DL.filter (\(plUID,plRID,_,_,_,_)-> plUID == userid && plRID == rrid) permissionList)  )
                                                    )  (DL.filter (\(ruid,_,_) -> ruid == userid && (case rid of
                                                                                                            Just _ -> True
                                                                                                            Nothing -> False)
                                                                                                        ) roleList)
                                                ) 
                                            ) rolePermissions
