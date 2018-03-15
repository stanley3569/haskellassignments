module NewConcepts2.GenExam2 where
import Data.List as DL  
import Data.Char
import Text.Read as TR
import Data.List.Split as DS
import Prelude 


import Data.Vector as DV
import Data.Text as DT
import Data.Csv as DC
--import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as DB
import Data.String.Conv

{-
score=[ ("Mojo", [("English", 84), ("Chemistry", 81), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)])
    , ("Captain Jack", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Dexter", [("Tamil", 110), ("Biology", 100), ("Geography", 100), ("Physics", -55)]) -- Note that marks in "Tamil" are greater than 100
    ]
-}
   


validsubject :: [String]
validsubject = ["English", "Geography", "Physics", "Chemistry", "Economics", "Computer Science"]

type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)         


validateMarks :: InputScore -> [(String, [(String, Int, Bool, String)])]
validateMarks score =
    let validmarks = DL.map (\(studentName,scorenames)->(studentName,             
                        DL.concatMap(\(subjectname,marks) ->
                            if (marks < 0)
                                then [(subjectname,marks,False,"negative marks")]
                            else if (marks > 100)
                                then [(subjectname,marks,False,"marks higher than 100")]
                            else if(subjectname `DL.elem` validsubject)
                                then [(subjectname,marks,True,"valid Subject")]
                            else [(subjectname,marks,False,"unknown Subject")]  ) scorenames )) score
        in validmarks



validateMarksSubjects ::(b->  (StudentName,Bool,[(Subject,Marks,Bool,String)]) -> b ) ->b -> InputScore -> ValidSubjects -> b
validateMarksSubjects testfunc accumulator score vsubjects= 
    let dupicatenames = (duplicateStudents score vsubjects)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = validateMarks score
        newscore =  DL.map(\(studentname,subjectinfo) -> 
                            if (studentname `DL.elem` dupicatenames) 
                                then (studentname,False,subjectinfo) 
                            else (studentname,True,subjectinfo)) validMarks
    in DL.foldl' (\x y -> testfunc x y) accumulator newscore 



type DuplicateStudentList =  [String] 
                                                                                        
duplicateStudents :: InputScore -> ValidSubjects -> DuplicateStudentList
duplicateStudents score vsubjects = 
    let studentnames = DL.map (\ x  ->  fst x ) score
        duplicatenames = DL.concatMap(\x -> if(DL.length x > 1) then [DL.head x] else [] ) (DL.group(DL.sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        in duplicatenames


invalidScores :: InputScore -> ValidSubjects  -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score vsubjects =  
        let invalidData3 = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->                         
                                newlist DL.++ [(studentname, (DL.concatMap (\(subjectname,marks,validsubject,validmessage)->
                                            if(validsubject==False)
                                                then [(subjectname,marks,validmessage)]
                                            else [] )) subjectinfo)] ) [] score vsubjects
            in invalidData3


averageMarks :: InputScore -> Subject -> ValidSubjects -> Float
averageMarks score subjectName vsubject =
    let markslist = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                                newlist DL.++ (DL.concatMap (\(subjectname,marks,validsubject,validmessage)->
                                    if(validstudent==True && subjectname==subjectName && validsubject == True)
                                        then [marks]
                                    else [] )) subjectinfo 
                                ) [] score vsubject
        averageSubject =  ( fromIntegral( DL.sum( markslist) ) / fromIntegral(DL.length markslist))                                          
     in averageSubject



standardDeviation :: InputScore -> Subject -> ValidSubjects -> Float
standardDeviation score subjectName vsubject = 
    let averagevalue = averageMarks score subjectName vsubject                              
        listsub = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist DL.++ (DL.concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True && subjectname==subjectName && validsubject == True)
                                then [marks]
                            else [] )) subjectinfo 
            ) [] score  vsubject
        calculateXM = ( DL.zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = DL.sum (DL.map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (DL.length(listsub) ) )   )  


------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [("English", []),("Geography", []),("Physics", []),("Chemistry", []),("Economics", []),("Computer Science", [])]



-------------------------------------------

studentsListForExam1 :: InputScore -> ValidSubjects -> [(Subject, [StudentName])]
studentsListForExam1 score vsubject= 
    let listsub =validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist DL.++ (DL.concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True && validsubject == True)
                                then [(subjectname,studentname)]
                            else [] )) subjectinfo 
            ) [] score vsubject
                                                                                                        --[("English","Mojo"),("Chemistry","Mojo"),("Physics","Mojo"),("Geography","Mojo"),("Chemistry","Captain Jack"),("Geography","Captain Jack"),("Geography","Dexter")]                   
        studentlist = DL.map (\z -> DL.foldl' (\ (subjectname,studentname) (x,y) ->
                                            if(x == subjectname)
                                                then (subjectname,studentname DL.++ [y])
                                            else (subjectname,studentname)   )
                                        z listsub ) validSubjectsStudentList 
    in studentlist
                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]




grouping :: InputScore -> ValidSubjects -> [([Subject], [StudentName])]
grouping score vsubjects = 
    let listofstudent = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        if(validstudent==True)
                            then newlist DL.++ [(studentname,DL.concatMap(\(subjectname,marks,validsubject,validmessage) -> 
                                    if validsubject
                                        then [subjectname]
                                    else []
                                    ) subjectinfo)]
                        else newlist
                        ) [] score vsubjects

                                                                                                                        --[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]                                            --1--[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]  
        subjectStudentList = studentsListForExam1 score vsubjects 
                                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]       
        subjectListbynames = delete [] (nub(DL.map (\(y,z)-> z) listofstudent) )                           
        namesListBysub =  (nub(DL.map(\(y,z)-> z) subjectStudentList))

        in DL.zip (subjectListbynames) namesListBysub 


--------------------------------------------------



pad :: Int -> String -> String
pad num xs = " " DL.++ xs DL.++ (DL.replicate (num-(DL.length xs) ) ' ')

forDuplicates :: Score -> ValidSubjects -> IO()
forDuplicates score vsubject = Prelude.putStrLn (DL.foldl' (\arr name -> arr DL.++ name DL.++ "\n") "" (duplicateStudents score vsubject))

forAverage :: Score -> ValidSubjects -> IO()
forAverage score vsubject=
    Prelude.putStrLn "Enter the subject name" >> Prelude.getLine >>= \subjectname -> if(subjectname `DL.elem` vsubject )
                                                                        then print (averageMarks score subjectname vsubject)
                                                                     else Prelude.putStrLn "Invalid SubjectName" >> forAverage score vsubject

forStdDeviation :: Score -> ValidSubjects -> IO()
forStdDeviation score vsubject = 
    Prelude.putStrLn "Enter the subject name" >> Prelude.getLine >>= \ subjectname -> if(subjectname `DL.elem` vsubject )
                                                                        then print (standardDeviation score subjectname vsubject)
                                                                      else Prelude.putStrLn "Invalid SubjectName" >> forStdDeviation score vsubject


forInvalidScores :: Score -> ValidSubjects -> IO()
forInvalidScores score vsubject = Prelude.putStrLn (DL.foldl' (\arr1 (student,validation)-> arr1 DL.++ 
    DL.foldl' (\str (subject,marks,reason)-> str DL.++ pad 20 student DL.++ " | " DL.++ pad 20 subject DL.++ " | " DL.++ pad 20 (show marks) DL.++ " | " DL.++ pad 20  reason DL.++ " | " DL.++"\n")
                    [] validation ) [] (invalidScores score vsubject) )



forStudentsListForExam1 :: Score -> ValidSubjects -> IO()
forStudentsListForExam1 score vsubject = 
    Prelude.putStrLn (DL.foldl' (\arr1 (subjectname,list)-> arr1 DL.++ "\n" DL.++ pad 20 subjectname DL.++ " | " DL.++ 
            DL.foldl' (\arr2 (student)-> arr2 DL.++ pad 20 student DL.++ " ") [] list )
                     [] (studentsListForExam1 score vsubject))

----------------------------------------



inputFile :: IO ()
inputFile = do
    Prelude.putStrLn "Enter the score file"
    scorefile <- Prelude.getLine 
    myscorefile <- ( DB.readFile scorefile)             --( BL.readFile scorefile)
    Prelude.putStrLn "Enter the subject filename"
    subjectfile <- Prelude.getLine
    mysubjectfile <- (DB.readFile subjectfile)
    main1 (toS myscorefile) mysubjectfile 





--main1 :: ByteString -> ByteString -> IO()
main1 :: BL.ByteString -> DB.ByteString -> IO ()
main1 myscorefile mysubjectfile = do 
    Prelude.putStrLn "Enter \n 1. Average \n 2. Duplicate \n 3. Std Deviation \n 4. Invalid scores \n 5. Students for exam \n 6. Exit"
    choice <- Prelude.getLine 
    let fscore =  scoreRecord1 (myscorefile)
        fsubject =  formatSubjects (DB.unpack mysubjectfile)
    case fscore of
        Right fscore1 -> 
                        case ((readMaybe choice)::Maybe Int) of
                                Just 1 -> forAverage fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 2 -> forDuplicates fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 3 -> forStdDeviation fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 4 -> forInvalidScores fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 5 -> forStudentsListForExam1 fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 6 -> Prelude.putStrLn "Exit"
                                _ -> main1 myscorefile mysubjectfile
        Left err -> do
                    Prelude.putStrLn err
                    inputFile








type Score = [(String,[(String,Int)])] 

--formatSubjects :: ByteString -> [String]
--formatSubjects :: ByteString ->Either String (Vector [String])









scoreRecord1 :: BL.ByteString -> Either String Score
scoreRecord1 inString = 
    let finalop = formatScore inString
        in case finalop of 
                Left err -> Left err
                Right score1 ->Right score1



formatScore :: BL.ByteString -> Either String [(String, [(String, Int)])]
formatScore scoreString = 
            let scoredata = DC.decode NoHeader (toS scoreString) :: (Either String (Vector  (String,String,String)) )
            in case scoredata of
                Left err -> Left err
                Right score1 -> 
                        let scorelist = DV.map (\(name1,subject1,marks1) -> [name1,subject1,marks1] ) score1
                            grouping1 = DL.groupBy  (\a  x ->  (DL.head a) ==  (DL.head x ) ) (DV.toList scorelist) 
                         in Prelude.mapM (\ records -> function1 records ("",[]) ) grouping1

{-}
formatScore:: ByteString -> Either String [(String, [(String, Int)])]
formatScore scoreString =
    let splitNewLine = DS.splitOn "\n" (scoreString)
        splitList =  DL.map (\x -> DS.splitOn "," x) splitNewLine
        grouping1 = DL.groupBy  (\a  x ->  (DL.head a) ==  (DL.head x ) ) splitList 

        in Prelude.mapM (\ records -> function1 records ("",[]) ) grouping1
-}



formatSubjects :: String -> [String]
formatSubjects subjectString = 
        DS.splitOn "," subjectString



function1:: [[String]] -> (String, [(String, Int)]) -> Either String (String, [(String, Int)])
function1 records (name,score1) =
    if(records == [])
        then Right (name,score1)
    else case (DL.head records) of
            ([]:_:_:[]) -> Left "Invalid name"
            (_:[]:_:[]) -> Left "Invalid subject"
            (_:_:[]:[]) -> Left "Invalid marks"
            (sname:ssubject:smarks:[]) -> case ((readMaybe smarks) :: Maybe Int) of
                                                Just mark1 -> if name==sname
                                                                then function1 (DL.tail records) (sname,score1 DL.++ [(ssubject,mark1)])
                                                               else function1 (DL.tail records) (name,score1 DL.++ [(ssubject,mark1)])
                                                Nothing -> Left "invalid marks"
            _ -> Left "Invalid"
