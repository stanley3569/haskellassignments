module Chapter3.RailFenceCipher1 where
    
    
    data Rails = MkRails{ dtRail::String} deriving (Eq, Show,Ord)
    
    data StartRails = MkStartRails {dtRail1:: String, dtRail2:: String,dtRail3:: String } deriving (Eq, Show,Ord)
    
    data CurrentRailMovementDirection = MkCurrentRailMovementDirection{ dtCurrentRail::Int, dtMovementDirection:: Int } deriving (Eq,Show,Ord)
    
    withoutSpaces :: String -> String
    withoutSpaces xs = (filter (/=' ') xs)
    
    
    
    encodeMessage :: String -> StartRails -> CurrentRailMovementDirection -> Rails 
    encodeMessage inputString (MkStartRails rail1 rail2 rail3) (MkCurrentRailMovementDirection currentRail movementDirection) = 
                                                                                if(inputString /= "") 
                                                                                    then  
                                                                                        if(currentRail == 1)
                                                                                            then encodeMessage (tail inputString) (MkStartRails (rail1++[(head inputString)]) rail2 rail3) (MkCurrentRailMovementDirection(currentRail-movementDirection) (movementDirection) )
                                                                                        else if(currentRail ==2)
                                                                                            then encodeMessage (tail inputString) (MkStartRails rail1 (rail2++[(head inputString)]) rail3) (MkCurrentRailMovementDirection(currentRail-(movementDirection)) (-movementDirection) )
                                                                                        else encodeMessage (tail inputString) (MkStartRails rail1 rail2 (rail3++[(head inputString)])) (MkCurrentRailMovementDirection(currentRail-1) (movementDirection) )
                                                                                else MkRails( rail1++rail2++rail3)
                                                                               
      
    
    main = do 
            print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") (MkStartRails [] [] []) (MkCurrentRailMovementDirection 1 (-1)))           --WECRLTEERDSOEEFEAOCAIVDEN
            print ( encodeMessage (withoutSpaces "helloworld") (MkStartRails [] [] []) (MkCurrentRailMovementDirection 1 (-1)) )                               --holelwrdlo
            print ( encodeMessage (withoutSpaces "hello") (MkStartRails [] [] []) (MkCurrentRailMovementDirection 1 (-1)) )                                   --hoell
            print ( encodeMessage (withoutSpaces "1234567890") (MkStartRails [] [] []) (MkCurrentRailMovementDirection 1 (-1)) )                               --1592468037
    
    