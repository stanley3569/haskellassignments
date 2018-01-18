module Chapter3.RailFenceCipher4 where


withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)
   


data StartRails1 = MkStartRails1 String deriving (Eq, Show,Ord)
data StartRails2 = MkStartRails2 String deriving (Eq, Show,Ord)
data StartRails3 = MkStartRails3 String deriving (Eq, Show,Ord)




encodeMessage :: String -> StartRails1 -> StartRails2 -> StartRails3 -> Int -> Int -> (StartRails1,StartRails2,StartRails3) 
encodeMessage inputString (MkStartRails1 rail1) (MkStartRails2 rail2) (MkStartRails3 rail3) currentRail movementDirection = 
                                                                            if(inputString /= "") 
                                                                                then  
                                                                                    if(currentRail == 1)
                                                                                        then encodeMessage (tail inputString) (MkStartRails1 (rail1++[(head inputString)]))  (MkStartRails2 rail2) (MkStartRails3 rail3) (currentRail-movementDirection) (movementDirection)
                                                                                    else if(currentRail ==2)
                                                                                        then encodeMessage (tail inputString) (MkStartRails1 rail1) (MkStartRails2 (rail2++[(head inputString)])) (MkStartRails3 rail3) (currentRail-(movementDirection)) (-movementDirection)
                                                                                    else encodeMessage (tail inputString) (MkStartRails1 rail1) (MkStartRails2 rail2) (MkStartRails3 (rail3++[(head inputString)])) (currentRail-1) (movementDirection)
                                                                            else ( (MkStartRails1 rail1),(MkStartRails2 rail2),(MkStartRails3 rail3) )
                                                                           
                                                                              
main = do
        print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") (MkStartRails1 []) (MkStartRails2 []) (MkStartRails3 []) 1 (-1))           --WECRLTEERDSOEEFEAOCAIVDEN
        print ( encodeMessage (withoutSpaces "helloworld") (MkStartRails1 []) (MkStartRails2 []) (MkStartRails3 []) 1 (-1))                               --holelwrdlo
        print ( encodeMessage (withoutSpaces "hello") (MkStartRails1 []) (MkStartRails2 []) (MkStartRails3 []) 1 (-1))                                    --hoell
        print ( encodeMessage (withoutSpaces "1234567890") (MkStartRails1 []) (MkStartRails2 []) (MkStartRails3 []) 1 (-1))                               --1592468037
