module Chapter3.RailFenceCipher3 where
withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)

data Rail1 = MkRail1 String deriving (Eq,Show,Ord)
data Rail2 = MkRail2 String deriving (Eq,Show,Ord)
data Rail3 = MkRail3 String deriving (Eq,Show,Ord)

encodeMessage :: String -> Rail1 -> Rail2 -> Rail3 -> Int -> Int -> String 
encodeMessage inputString (MkRail1 rail1) (MkRail2 rail2) (MkRail3 rail3) currentRail movementDirection = 
                                                                            if(inputString /= "") 
                                                                                then  
                                                                                    if(currentRail == 1)
                                                                                        then encodeMessage (tail inputString) (MkRail1 (rail1++[(head inputString)]) ) (MkRail2 rail2) (MkRail3 rail3) (currentRail-movementDirection) (movementDirection)
                                                                                    else if(currentRail ==2)
                                                                                        then encodeMessage (tail inputString) (MkRail1 rail1) (MkRail2 (rail2++[(head inputString)]) ) (MkRail3 rail3) (currentRail-(movementDirection)) (-movementDirection)
                                                                                    else encodeMessage (tail inputString) (MkRail1 rail1) (MkRail2 rail2) (MkRail3 (rail3++[(head inputString)]) ) (currentRail-1) (movementDirection)
                                                                            else rail1++rail2++rail3
                                                                           
                                                                               
main = do
        print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") (MkRail1 []) (MkRail2 []) (MkRail3 []) 1 (-1))           --WECRLTEERDSOEEFEAOCAIVDEN
        print ( encodeMessage (withoutSpaces "helloworld") (MkRail1 []) (MkRail2 []) (MkRail3 []) 1 (-1))                               --holelwrdlo
        print ( encodeMessage (withoutSpaces "hello") (MkRail1 []) (MkRail2 []) (MkRail3 []) 1 (-1))                                    --hoell
        print ( encodeMessage (withoutSpaces "1234567890") (MkRail1 []) (MkRail2 []) (MkRail3 []) 1 (-1))                               --1592468037