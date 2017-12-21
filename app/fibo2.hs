evensum :: Int -> Int -> Int -> Int -> Int
evensum maxValue a b total = if(b < maxValue)
                                then
                                    if (b `mod` 2) == 0
                                        then evensum maxValue b (a+b) (total+b)
                                    else evensum maxValue b (a+b) total
                              else total