module Functors.Test6 where

import Prelude hiding (lookup)
import Data.Map.Strict as Map
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
userList =  [(100,"mojo jo"),
            (101,"dexter"),
            (102,"goku"),
            (103,"sam"),(104,"dave"),
            (105,"wolfgang"),
            (106,"aatish"),
            (107,"Kahlil"),
            (2765,"wolfgang@vacationlabs.com"),
            (2766,"rajdeep+agent@vacationlabs.com"),
            (1041,"rajdeepm@vacationlabs.com"),
            (1429,"supplier-5-content@vacationlabs.com"),
            (2393,"aaditya@vacationlabs.com"),
            (2638,"shubham.k@vacationlabs.com"),
            (2776,"anuj+wayne@vacationlabs.com"),
            (2775,"anuj+nair@vacationlabs.com"),
            (2777,"anuj+barry@vacationlabs.com"),
            (2778,"anuj+clark@vacationlabs.com") ]

test1 :: Int -> Maybe String
test1 iid = 
    lookup iid (fromList userList)

-- [(userId, permissionId, action, class, description)]
individualPermissions :: [(Int, Int, String, String, String)]
individualPermissions = 
    [(101, 2, "manage_calendar ", "Trips::Trip ", "Allowed to edit Departure calendar"),
     (101,9,"manage_cancellation_email","Trips::Trip","Allowed to edit cancellation email"),
     (103,46,"manage_agents ","Common::Client ","Allowed to manage agent clients"),
     (102,22,"override_capacity_constraints ","Itinerary::Booking ","Allowed to override booking capacity constraints"),
     (102,2,"manage_calendar ", "Trips::Trip ", "Allowed to edit Departure calendar"),
     (104,3,"manage_content","Trips::Trip","Allowed to edit trip content"),
     (105,41,"manage_passenger_report ","Reports::PassengerReport "," Allowed to manage passenger reports"),
     (2765,12,"manage_rejection_email","Trips::Trip","Allowed to edit rejection email"),
     (2766,20 ,"edit_booking_txn","Itinerary::Booking","Allowed to edit taxable items in a booking like rates, payments, coupons etc"),
     (1041,21 ,"edit_booking_non_txn","Itinerary::Booking","Allowed to edit non-taxable items in a booking like comments, contact-details etc"),
     (2775,23,"confirm_manual_bookings","Itinerary::Booking","Allowed to confirm/reject bookings with pending confirmation"),
     (2765,25,"manage_reports","Itinerary::Booking","Allowed to manage booking reports"),
     (2765,30,"manage_settings","Common::Client","Allowed to manage general settings like support details, branding etc"),
     (2775,11,"manage_pending_confirmation_email","Trips::Trip","Allowed to edit pending confirmation email")]

-- [(userId, roleId, roleName)]
roleList :: [(Int, Int, String)]
roleList = [(100,1,"Basic User"),
            (102,5,"Reservation Manager"),
            (103,3," Content manager "),
            (104,2,"Product manager"),
            (105,4,"Reservation staff "),
            (2765,6,"Finance manager"),
            (2775,7,"Super user"),
            (2766,8,"Event crew"),
            (101,9,"Supplier"),
            (102,2,"Product Manager")]


-- [(userId, roleId, permissionId)]
rolePermissions :: [(Int,Maybe Int, Maybe Int)]
rolePermissions = [(100,Just 1,Nothing),
                    (102,Just 3,Nothing),
                    (103,Just 2,Nothing),
                    (104,Just 1,Nothing),
                    (105,Just 3,Nothing),
                    (2765,Just 2,Nothing),
                    (2775,Just 1,Nothing),
                    (2766,Just 3,Nothing),

                    (101,Nothing,Just 9),
                    (102,Nothing,Just 2),
                    (100,Nothing,Just 1),
                    (102,Nothing,Just 5),
                    (103,Nothing,Just 3),
                    (104,Nothing,Just 2),
                    (105,Nothing,Just 4),
                    (2765,Nothing,Just 6),
                    (2775,Nothing,Just 7),
                    (2766,Nothing,Just 8),
                    (101,Nothing,Just 9),
                    (102,Nothing,Just 2)
                    ]


-- [(userId, roleId, permissionId, action, class, description)]
permissionList :: [(Int, Int, Int, String, String, String)]
permissionList = [(101,2,1,"manage_calendar","Trips::Trip","Allowed to edit Departure calendar"),
                   (102,3,5,"override_capacity_constraints ","Itinerary::Booking ","Allowed to override booking capacity constraints"),
                   (103,2,3,"manage_agents ","Common::Client ","Allowed to manage agent clients"),
                   (104,1,2,"manage_content","Trips::Trip","Allowed to edit trip content"),
                   (105,3,4,"manage_passenger_report ","Reports::PassengerReport "," Allowed to manage passenger reports"),
                   (101,1,9,"manage_cancellation_email","Trips::Trip","Allowed to edit cancellation email"),
                   (102,2,2,"manage_calendar ", "Trips::Trip ", "Allowed to edit Departure calendar")
                   ]


--prepareUITable :: UITable                 --Int -> Maybe [Char]
--testTable :: Int -> Maybe String

{-}
prepareUITable xs = 
    let x = (Map.filter (==xs) (fromList userList) )
       in x



---get id for a certain email
testTable id =
    let email = maybeToList $ lookup id (fromList userList)
        t1 =fmap (\(a,b,c,d,e,f) -> if(a==id)                              -- [(userId, roleId, permissionId, action, class, description)]
                                        then Just (a,b,c,d,f)
                                     else Nothing ) permissionList
        t2 = fmap (\x -> case x of
                            Nothing -> 0
                            Just (a,b,c,d,e) -> b ) t1
    in t2
-}
--[(userId, roleId, roleName)]
test2 = 
    let t1= (fmap (\(userid,email) -> (email,
            [(Prelude.filter (not . Prelude.null) (fmap (\(uid,rid,rname)-> if(uid==userid) then rname else []) roleList)),
            (Prelude.filter (not . Prelude.null)  (fmap (\(uid,roleid,perid,action,clas,des) -> 
                                                                            if(userid==uid) 
                                                                                then des
                                                                            else []) permissionList) )]
            )  )userList)
        
    in t1

{-}
test2x =
    let t1= (fmap (\(userid,email) -> (email,
            [(Prelude.filter (not . Prelude.null) (fmap (\(uid,rid,rname)-> if(uid==userid) then (rname,
            (Prelude.filter (not . Prelude.null)  (fmap (\(uid,roleid,perid,action,clas,des) -> 
                                                                            if(userid==uid) 
                                                                                then des
                                                                            else []) permissionList) )) else (rname,[]) roleList))]
            )  )userList)
        
    in t1
-}
{-}

test3 userid = 
    let t1 =Prelude.filter (not . Prelude.null) (fmap (\(x,y,z)-> if(x==userid) then  z else []) roleList)
    in t1



-- [(userId, roleId, permissionId)]
--rolePermissions :: [(Int,Maybe Int, Maybe Int)]
--rolePermissions = [(100,Just 1,Nothing),

test4 userid=concat $ Prelude.filter (not . Prelude.null) ( fmap (\(x,y,z)-> if(x==userid)
                                    then case y of
                                            Just a -> [a]
                                            Nothing -> []
                                else []
                     ) rolePermissions)
-- [(userId, roleId, permissionId, action, class, description)]

test5 userid = (Prelude.filter (not . Prelude.null)  (fmap (\(uid,roleid,perid,action,clas,des) -> 
                                                        if(userid==uid) 
                                                            then des
                                                        else []) permissionList) )







-- [(userId, permissionId, action, class, description)]
test6 userid = (Prelude.filter (not . Prelude.null)  (fmap (\(uid,pid,action,clas,des) -> 
                                if(userid==uid) 
                                    then des
                                else []) individualPermissions) )

-}




