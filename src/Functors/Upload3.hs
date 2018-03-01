module Functors.Upload3 where
    
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







prepareUITable :: UITable
prepareUITable = 
    fromList (DL.map (\(uid1,email,pid1,_,_,_,rid1,_,_,_,_,_)->(
                    User {userId=uid1,userEmail=email},( (permissionIndividual uid1 pid1),(rolePermission uid1 rid1 )
                        ))) joinResults)
        where
        permissionIndividual uid1 pid1 =
                case pid1 of
                    Nothing -> mempty
                    Just iPid -> DL.foldl' (\arr x -> 
                        case x of                           
                        (uid2,_,Just pid2,Just pact2,Just pclas2,Just pdes2,_,_,_,_,_,_)->
                            if (iPid == pid2 && uid1 == uid2)
                                then arr++[Permission {permissionId=pid2,permissionAction=pact2,permissionClass=pclas2,permissionDescripton=pdes2}]
                            else arr

                        _-> arr ) mempty joinResults
        
        rolePermission uid1 rid1 =
                case rid1 of
                    Nothing -> fromList mempty
                    Just roleid -> fromList (DL.foldl' (\arr1 x ->
                        case x of
                            (uid3,_,_,_,_,_,Just rrid1,Just rname1,_,_,_,_) ->

                                if (uid1 == uid3 && roleid == rrid1)
                                    then arr1++[(Role {roleId=rrid1,roleName=rname1},DL.foldl' (\arr2 y->
                                                                    case y of 
                                                                    (uid4,_,_,_,_,_,Just rid4,_,Just rpid4,Just ract4,Just rclas4,Just rdes4)->
                                                                        if (uid1 == uid3 && uid3 == uid4 && roleid==rrid1 && rid4 == rrid1)
                                                                            then arr2++ [Permission {permissionId=rpid4,permissionAction=ract4,permissionClass=rclas4,permissionDescripton=rdes4}]
                                                                        else arr2
                                                                    _->arr2 ) mempty joinResults)]

                                else arr1
                            _->arr1 ) mempty joinResults)


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
                                        (DL.foldl' (\arr2p Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->(arr2p++ (pad 30 ( des ) ) )) mempty rolperm)
                                    )) mempty (Map.toList roles)++ " | " ++
                     (DL.foldl' (\ppstr Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=des}->ppstr++(pad 70 "") ++ (pad 40 ( des ) )++ " | ") mempty inperm ) ++ " \n "
-}

pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')


--showTable :: IO()
--showTable = putStr (display (prepareUITable))




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
                        else ipermSet ++ (DL.map (\_-> (pad 50 "" )++"\n") [1..(maxR-maxI)])
                        where ipermSet = DL.map (\x->(pad 50 x)++"\n") iperms

            in zip3 emailDisplay rolNamePer indPer 
            ) (showUITable uiTable)


showTable1:: IO()
showTable1 = putStr (DL.foldl' (\ac x->DL.foldl' (\arr (email,(role,rolePer),iPer)-> arr ++ (email++ " | " ++role++ " | "++pad 40 rolePer++ " | " ++iPer)) ac x) "" (display1 (prepareUITable)))


















{-
fromList [
    (User {userId = 1041, userEmail = "user3@gmail.com"},([],fromList [])),
    (User {userId = 1429, userEmail = "user4@gmail.com"},([],fromList [(Role {roleId = 2, roleName = "Product manager"},[Permission {permissionId = 6, permissionAction = "manage_confirmation_email ", permissionClass = "Trips::Trip", permissionDescripton = "Allowed to edit confirmation email"}])])),
    (User {userId = 2765, userEmail = "user1@gmail.com"},([Permission {permissionId = 3, permissionAction = "manage_post_trip_email", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit post trip email"}],fromList [])),
    (User {userId = 2766, userEmail = "user2@gmail.com"},([Permission {permissionId = 8, permissionAction = "manage_post_trip_email", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit post trip email"}],fromList []))]


-}



{-}


testingdisplay222 :: [(String, ([String], [(String, [String])]))]
testingdisplay1 = group ( (displayUITable1  prepareUITable) )

displayUITable1:: Map User ([Permission], Map Role [Permission]) -> [(String, String, String, String)]
displayUITable1 input1 =
            foldlWithKey (\ks k x ->  (case k of
                User {userId = uid,userEmail = uemail} -> (uemail ), (t1 x) ,(t3 x), (test1  x) 
                        ):ks ) 
                 mempty input1
            where 
                t1 x =t2 (case x of
                            (_,b)->head $ toList b) 
                t2 x1= case x1 of                                                                                                                -- (Role {roleId=rid,roleName=rname},[Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pd}])-> (rname,pd)
                        (Role {roleId=_,roleName=rname},_)-> rname
                t3 x = case (case x of (a,b)->head $ toList b
                                        ) of
                            (Role {roleId=rid,roleName=rname},[Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pd}])-> pd

                test1 x = case x of
                                (a,_)-> case a of
                                            [Permission {permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}]->pdes
                                            []->[]


-}






























--prepareUITable22 :: UITable

{-}
prepareUITable22 = 
    fromList (DL.map (\(uid1,email,pid1,_,_,_,rid1,_,_,_,_,_)->(
                    User {userId=uid1,userEmail=email},( 
                            (permissionIndividual uid1 pid1)
                        ))) joinResults)
        where
            permissionIndividual uid1 pid1 = 
                maybe [] [(123,Just 2765)] pid1

-}


                --Data.Maybe.fromJust $ Map.lookup uid1 joinResults
            
            --maybe [] ( (\ x -> x
            --                         ) joinResults) pid1
            --fromJust $ DL.lookup pid1 joinResults


            {-}
                case pid1 of
                    Nothing -> mempty
                    Just iPid -> DL.foldl' (\arr x -> 
                        case x of                           
                        (uid2,_,Just pid2,Just pact2,Just pclas2,Just pdes2,_,_,_,_,_,_)->
                            if (iPid == pid2 && uid1 == uid2)
                                then arr++[Permission {permissionId=pid2,permissionAction=pact2,permissionClass=pclas2,permissionDescripton=pdes2}]
                            else arr

                        _-> arr ) mempty joinResults
        -}


{-
--prepareUITable22 :: UITable
prepareUITable22 = DL.foldl' (\accMap (usrId,usrEmail,_,_,_,_,_,_,_,_,_,_)-> 
    let indvP= DL.foldl' (\iperms (_,_,Just fiperId,Just fiperAct,Just fiperCls,Just fiperDes,_,_,_,_,_,_)->
                            iperms++[(Permission {permissionId = fiperId,permissionAction=fiperAct,permissionClass=fiperCls,permissionDescripton=fiperDes})]
                        ) [] (DL.filter (\(fuid,_,fpid,fpact,fpcls,fpdes,_,_,_,_,_,_)->fuid==usrId && 
                            case (fpid,fpact,fpcls,fpdes) of
                                (Just _,Just _,Just _,Just _) -> True
                                _ -> False
                        ) joinResults)

    
                        in Map.insert (User {userId=usrId,userEmail=usrEmail}) (
                            (indvP)
                        ) accMap 
                    ) Map.empty joinResults




 fromList [
     (User {userId = 1041, userEmail = "user3@gmail.com"},[]),
     (User {userId = 1429, userEmail = "user4@gmail.com"},[]),
     (User {userId = 2765, userEmail = "user1@gmail.com"},[Permission {permissionId = 2, permissionAction = "manage_calendar ", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit Departure calendar"},Permission {permissionId = 2, permissionAction = "manage_post_trip_email", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit post trip email"}]),
     (User {userId = 2765, userEmail = "user2@gmail.com"},[Permission {permissionId = 2, permissionAction = "manage_calendar ", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit Departure calendar"},Permission {permissionId = 2, permissionAction = "manage_post_trip_email", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit post trip email"}]),
     (User {userId = 2766, userEmail = "user2@gmail.com"},[Permission {permissionId = 8, permissionAction = "manage_post_trip_email", permissionClass = "Trips::Trip", permissionDescripton = " Allowed to edit post trip email"}])]       




     -}



--prepareUITable22 :: UITable
prepareUITable22 = 
    fromList (DL.map (\(uid1,email,pid1,_,_,_,rid1,_,_,_,_,_)->(
                    User {userId=uid1,userEmail=email},( (permissionIndividual uid1 pid1)
                        ))) joinResults)
        where
        permissionIndividual uid1 pid1 =
                case pid1 of
                    Nothing -> mempty
                    Just iPid -> DL.foldl' (\arr x -> 
                        case x of                           
                        (uid2,_,Just pid2,Just pact2,Just pclas2,Just pdes2,_,_,_,_,_,_)->
                            if (iPid == pid2 && uid1 == uid2)
                                then arr++[Permission {permissionId=pid2,permissionAction=pact2,permissionClass=pclas2,permissionDescripton=pdes2}]
                            else arr

                        _-> arr ) mempty (DL.filter (\ (uid4,email4,pid4,_,_,_,rid4,_,_,_,_,_)-> uid1==uid4 && pid4==pid1 )   joinResults)

        rolePermission uid1 rid1 =
                case rid1 of
                    Nothing -> fromList mempty
                    Just roleid -> fromList (DL.foldl' (\arr1 x ->
                        case x of
                            (uid3,_,_,_,_,_,Just rrid1,Just rname1,_,_,_,_) ->

                                if (uid1 == uid3 && roleid == rrid1)
                                    then arr1++[(Role {roleId=rrid1,roleName=rname1},DL.foldl' (\arr2 y->
                                                                    case y of 
                                                                    (uid4,_,_,_,_,_,Just rid4,_,Just rpid4,Just ract4,Just rclas4,Just rdes4)->
                                                                        if (uid1 == uid3 && uid3 == uid4 && roleid==rrid1 && rid4 == rrid1)
                                                                            then arr2++ [Permission {permissionId=rpid4,permissionAction=ract4,permissionClass=rclas4,permissionDescripton=rdes4}]
                                                                        else arr2
                                                                    _->arr2 ) mempty joinResults)]

                                else arr1
                            _->arr1 ) mempty (DL.filter (\ (uid5,email5,pid5,_,_,_,rid5,_,_,_,_,_)-> (uid1==uid5 && rid1==rid5) )   joinResults))









prepareUITable23 :: UITable
prepareUITable23 = 
    fromList (DL.map (\(uid1,email,pid1,_,_,_,rid1,_,_,_,_,_)->(
                    User {userId=uid1,userEmail=email},( (permissionIndividual uid1 pid1),(rolePermission uid1 rid1 )
                        ))) joinResults)
        where
        permissionIndividual uid1 pid1 =
                case pid1 of
                    Nothing -> mempty
                    Just iPid -> DL.foldl' (\arr x -> 
                        case x of                           
                        (uid2,_,Just pid2,Just pact2,Just pclas2,Just pdes2,_,_,_,_,_,_)->
                            if (iPid == pid2 && uid1 == uid2)
                                then arr++[Permission {permissionId=pid2,permissionAction=pact2,permissionClass=pclas2,permissionDescripton=pdes2}]
                            else arr

                        _-> arr ) mempty joinResults
        
        rolePermission uid1 rid1 =
                case rid1 of
                    Nothing -> fromList mempty
                    Just roleid -> fromList (DL.foldl' (\arr1 x ->
                        case x of
                            (uid3,_,_,_,_,_,Just rrid1,Just rname1,_,_,_,_) ->

                                if (uid1 == uid3 && roleid == rrid1)
                                    then arr1++[(Role {roleId=rrid1,roleName=rname1},DL.foldl' (\arr2 y->
                                                                    case y of 
                                                                    (uid4,_,_,_,_,_,Just rid4,_,Just rpid4,Just ract4,Just rclas4,Just rdes4)->
                                                                        if (uid1 == uid3 && uid3 == uid4 && roleid==rrid1 && rid4 == rrid1)
                                                                            then arr2++ [Permission {permissionId=rpid4,permissionAction=ract4,permissionClass=rclas4,permissionDescripton=rdes4}]
                                                                        else arr2
                                                                    _->arr2 ) mempty joinResults)]

                                else arr1
                            _->arr1 ) mempty joinResults)





testingdisplay222 =  ( (displayUITable  prepareUITable23) )

displayUITable :: UITable -> [(String, ([String], [(String, [String])]))]
displayUITable uiTable = DL.map (\(k,v)->(
    let User {userId=_,userEmail=email} = k
    in email
    ,
    let 
      (inperm,roles) = v
      ipermList = DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->pdes) inperm
    in (ipermList,
      DL.map (\(Role{roleId=_,roleName=rolname},rolperm)->(rolname,
      DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->pdes) rolperm
      )) (toList roles)
    )
    )) (toList uiTable)

