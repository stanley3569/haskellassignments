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
                (2638,"shubham.k@vacationlabs.com")         --,
               -- (2776,"anuj+wayne@vacationlabs.com"),
              --  (2775,"anuj+nair@vacationlabs.com"),
               -- (2777,"anuj+barry@vacationlabs.com"),
              --  (2778,"anuj+clark@vacationlabs.com")
                ]

--test2 = fromList (fmap (\x -> x) userList)
-- [(userId, permissionId, action, class, description)]
individualPermissions :: [(Int, Int, String, String, String)]
individualPermissions = [
                        ---(2765, 2,"manage_calendar","Trips::Trip","Allowed to edit Departure calendar"),
                        ---(2766, 45,"manage_agent_report","Reports::AgentReport","Allowed to manage agent reports"),
                        --(1041,46,"manage_agents","Common::Client","Allowed to manage agent clients"),
                        (1429,44,"edit_trip_confirmation_mode","Trips::Trip","Allowed to edit trip's confirmation mode"),
                        (2393,4,"manage_addons","Trips::Trip","Allowed to edit trip addons"),
                        (2765, 8,"manage_post_trip_email","Trips::Trip","Allowed to edit post trip email")    --,

                       -- (2638,8,"manage_post_trip_email","Trips::Trip","Allowed to edit post trip email"),
                       -- (2776,30,"manage_settings","Common::Client","Allowed to manage general settings like support details, branding etc."),
                       -- (2775, 1,"edit_essentials","Trips::Trip","Allowed to create a new trip and change basic details, settings and rates for all trips"),
                       -- (2777,1,"edit_essentials","Trips::Trip","Allowed to create a new trip and change basic details, settings and rates for all trips"),
                       -- (2778,1,"edit_essentials","Trips::Trip","Allowed to create a new trip and change basic details, settings and rates for all trips")

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
                        (1429,Nothing,Just 44),
                        (2393,Nothing,Just 4),
                        (2765,Nothing,Just 8)

                    ]


permissionList :: [(Int, Int, Int, String, String, String)]
permissionList = [
                    (2765,2,2,"manage_calendar","Trips::Trip","xxxxxAllowed to edit Departure calendar"),
                    (2766,3,45,"manage_agent_report","Reports::AgentReport","xxxxAllowed to manage agent reports"),
                    (1041,4,46,"manage_agents","Common::Client","xxxxxAllowed to manage agent clients"),         --,
                    (1429,5,44,"edit_trip_confirmation_mode","Trips::Trip","xxxxxAllowed to edit trip's confirmation mode"),
                    (2393,6,4,"manage_addons","Trips::Trip","xxxxxxAllowed to edit trip addons"),
                    (2765,9,8,"manage_post_trip_email","Trips::Trip","xxxxxxAllowed to edit post trip email")
                ]


testing4 = Map.fromList
        (fmap (\(userid,email)->( User {userId=userid,userEmail=email},( (t2 userid), (t3 userid)   ))) userList)
    where
        t2 userid= fmap (\(_,ppid,pact,pclas,pdes)-> Permission {permissionId=ppid,permissionAction=pact,permissionClass=pclas,permissionDescripton=pdes}
                    ) (DL.filter (\(puid,_,_,_,_)-> userid == puid) individualPermissions)

        t3 userid = fmap (\(_,rid,_)->
                    Map.fromList (  
                        fmap (\ (ruid,rrid,rrname) -> ((Role {roleId=ruid,roleName=rrname},
                                fmap (\(_,_,ppid1,pact1,pclas1,pdes1)-> 
                                            Permission{permissionId=ppid1,permissionAction=pact1,permissionClass=pclas1,permissionDescripton=pdes1})
                                                            (DL.filter (\(plUId,plRolId,_,_,_,_)-> plUId == userid && plRolId == rrid) permissionList)))
                        )  (DL.filter (\(ruid,_,_) -> ruid == userid) roleList)
                    ) 
                )
                    rolePermissions

