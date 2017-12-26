module Chapter1.RailFenceCipher where
withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)




encodeMessage :: String -> String -> String -> String -> Int -> Int -> String 
encodeMessage inputString rail1 rail2 rail3 currentRail movementDirection = if((movementDirection == 1) && (inputString /= "") )
                                                                                then  
                                                                                      if(currentRail == 1)
                                                                                        then encodeMessage (tail inputString) (rail1++[(head inputString)]) rail2 rail3 (currentRail+1) 1
                                                                                      else if(currentRail ==2)
                                                                                        then encodeMessage (tail inputString) rail1 (rail2++[(head inputString)]) rail3 (currentRail+1) 1
                                                                                      else encodeMessage (tail inputString) rail1 rail2 (rail3++[(head inputString)]) (currentRail-1) 2
                                                                            else if((movementDirection ==2) && (inputString /= "") )
                                                                                then encodeMessage (tail inputString) rail1 (rail2++[(head inputString)]) rail3 (currentRail-1) 1
                                                                            else codeMessage rail1 rail2 rail3

codeMessage :: String -> String -> String -> String
codeMessage rail1 rail2 rail3 = (rail1 ++ rail2 ++ rail3 )




main = do
  print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") [] [] [] 1 1)

  print ( encodeMessage (withoutSpaces "helloworld") [] [] [] 1 1)