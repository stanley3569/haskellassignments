module Lens.Test1 where

import Control.Lens
----------------------------Understand lenses (using `^.`, `view`, `over`, `set`, `&`, `.~`)

 --   We view the first part of the tuple using view
--view _2 ("goal", "chaff")
--"chaff"



-- we're mapping our modification "over" the focal point of _1
--over _1 (++ "!!!") ("goal", "the crowd goes wild")
--("goal!!!","the crowd goes wild")


-- we can set the subpart to be a new subpart. This is called set
--set _1 "set" ("game", "match")
--("set", "match")


--("hello","world")^._2
--"world"

--set _2 42 ("hello","world")
--("hello",42)




--("hello",("world","!!!"))^._2._1
--"world"
--("hello",("world","!!!"))^._2._2
--"!!!"



--set (_2._1) 42 ("hello",("world","!!!"))
--("hello",(42,"!!!"))


-- "hello"^.to length
--5

-------A Getter works like a Lens, except you can only read from it, not write.

--("hello",("world","!!!"))^._2._2.to length
--3



----------(.~) is an infix alias for set.

-- _1 .~ "hello" $ ((),"world")
--("hello","world")

--((), "world") & _1 .~ "hello"
--("hello","world")



----------------------------

--import Data.Map as Map
--Map.fromList [("hello","there")] ^.at "hello"
--Just "there"



-------------You can insert
-- Map.fromList [("hello","there")] & at "hello" ?~ "world"
--fromList [("hello","world")]



-------------delete with this lens

--Map.fromList [("hello","there")] & at "hello" .~ Nothing
--fromList []





--------------over is then analogous to fmap, but parameterized on the Setter


--fmap succ [1,2,3]
--[2,3,4]
--over mapped succ [1,2,3]
--[2,3,4]





-----------The benefit is that you can use any Lens as a Setter,
        ---and the composition of setters with other setters or lenses using (.) yields a Setter.

--over (mapped._2) succ [(1,2),(3,4)]
--[(1,3),(3,5)]

--over (mapped._2) (+1) [(1,2),(1,3),(1,4)]
--[(1,3),(1,4),(1,5)]


------(%~) is an infix alias for ‘over’, 
    --and the precedence lets you avoid swimming in parentheses

--([(42, "hello")],"world") & _1.mapped._2.mapped %~ succ
--([(42, "ifmmp")],"world")




--each (\x -> print x >> return (-x)) (8, 19)


-------------      (.~)           Replace the target of a Lens or all of the targets of a Setter or Traversal with a constant value.
--(42,"world") & _1 .~ "hello"
--("hello","world")

--(a,b) & both .~ c
--(c,c)



