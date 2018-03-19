
module Lens.GenExam where
import Data.List as DL  
--import Data.Char
import Text.Read as TR
import Data.List.Split as DS
import Prelude 


import Data.Vector as DV
--import Data.Text as DT
import Data.Csv as DC
--import Data.ByteString as BS
import Data.ByteString.Lazy as BL
--import Data.ByteString.Char8 as DB
import Data.ByteString as DB
import Data.String.Conv
import Control.Lens


type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)         


validateMarks :: InputScore ->ValidSubjects -> [(String, [(String, Int, Bool, String)])]
validateMarks score vsubjects=
    let validmarks = over mapped (\(studentName,scorenames)->(studentName,             
                        DL.concatMap(\scorenames ->
                            if ( (scorenames^._2) < 0)
                                then [( (scorenames^._1),(scorenames^._2),False,"negative marks")]
                            else if ((scorenames^._2) > 100)
                                then [( (scorenames^._1),(scorenames^._2),False,"marks higher than 100")]
                            else if( (scorenames^._1) `DL.elem` vsubjects)
                                then [( (scorenames^._1),(scorenames^._2),True,"valid Subject")]
                            else [( (scorenames^._1),(scorenames^._2),False,"unknown Subject")]  ) scorenames )) score
        in validmarks



validateMarksSubjects ::(b->  (StudentName,Bool,[(Subject,Marks,Bool,String)]) -> b ) ->b -> InputScore -> ValidSubjects -> b
validateMarksSubjects testfunc accumulator score vsubjects= 
    let dupicatenames = (duplicateStudents score vsubjects)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = validateMarks score vsubjects
        newscore =  over mapped (\ studentInfo -> 
                            if ( (studentInfo^._1) `DL.elem` dupicatenames) 
                                then ( (studentInfo^._1),False,(studentInfo^._2)) 
                            else ((studentInfo^._1),True,(studentInfo^._2))) validMarks
    in DL.foldl' (\x y -> testfunc x y) accumulator newscore 



type DuplicateStudentList =  [String] 
                                                                                        
duplicateStudents :: InputScore -> ValidSubjects -> DuplicateStudentList
duplicateStudents score vsubjects = 
    let studentnames = over mapped (\ x  ->  (x^._1) ) score
        duplicatenames = DL.concatMap(\x -> if(DL.length x > 1) then (x^.._head) else [] ) (DL.group(DL.sort studentnames) )
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
    let markslist = validateMarksSubjects (\newlist studentDetails ->
                                newlist DL.++ (DL.concatMap (\ subjectDetails ->
                                    if((studentDetails^._2)==True && (subjectDetails^._1)==subjectName && (subjectDetails^._3) == True)
                                        then [(subjectDetails^._2)]
                                    else [] )) (studentDetails^._3) 
                                ) [] score vsubject
        averageSubject =  ( fromIntegral( DL.sum( markslist) ) / fromIntegral(DL.length markslist))                                          
     in averageSubject

--(studentname,validstudent,subjectinfo)
--(subjectname,marks,validsubject,validmessage)

standardDeviation :: InputScore -> Subject -> ValidSubjects -> Float
standardDeviation score subjectName vsubject = 
    let averagevalue = averageMarks score subjectName vsubject                              
        listsub = validateMarksSubjects (\newlist studentData ->
                        newlist DL.++ (DL.concatMap (\ subjectInfo ->
                            if((studentData^._2)==True && (subjectInfo^._1)==subjectName && (subjectInfo^._3) == True)
                                then [(subjectInfo^._2)]
                            else [] )) (studentData^._3)
            ) [] score  vsubject
        calculateXM = ( DL.zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = DL.sum (over mapped (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (DL.length(listsub) ) )   )  

--(studentname,validstudent,subjectinfo)
--(subjectname,marks,validsubject,validmessage)
------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [("English", []),("Geography", []),("Physics", []),("Chemistry", []),("Economics", []),("Computer Science", [])]



-------------------------------------------

studentsListForExam1 :: InputScore -> ValidSubjects -> [(Subject, [StudentName])]
studentsListForExam1 score vsubject= 
    let listsub =validateMarksSubjects (\newlist studentDetails ->
                        newlist DL.++ (DL.concatMap (\ subjectDetails ->
                            if((studentDetails^._2) ==True && (subjectDetails^._3) == True)
                                then [( (subjectDetails^._1),(studentDetails^._1) )]
                            else [] )) (studentDetails^._3) 
            ) [] score vsubject



                                                                                                        --[("English","Mojo"),("Chemistry","Mojo"),("Physics","Mojo"),("Geography","Mojo"),("Chemistry","Captain Jack"),("Geography","Captain Jack"),("Geography","Dexter")]                   
        studentlist = over mapped (\z -> DL.foldl' (\ subjectStudent info ->
                                            if( (info^._1) == (subjectStudent^._1) )
                                                then ( (subjectStudent^._1) , (subjectStudent^._2)  DL.++ [(info^._2)])
                                            else ( (subjectStudent^._1) , (subjectStudent^._2) )   )
                                        z listsub ) validSubjectsStudentList 
    in studentlist
                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]



grouping :: InputScore -> ValidSubjects -> [([Subject], [StudentName])]
grouping score vsubjects = 
    let listofstudent = validateMarksSubjects (\newlist studentInfo ->
                        if( (studentInfo^._2) ==True)
                            then newlist DL.++ [( (studentInfo^._1) ,DL.concatMap(\ subjectInfo -> 
                                    if (subjectInfo^._3)
                                        then [(subjectInfo^._1)]
                                    else []
                                    ) (studentInfo^._3) )]
                        else newlist
                        ) [] score vsubjects

--(studentname,validstudent,subjectinfo)
--(subjectname,marks,validsubject,validmessage)
                                                                                                                        --[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]                                            --1--[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]  
        subjectStudentList = studentsListForExam1 score vsubjects 
                                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]       
        subjectListbynames = delete [] (nub(over mapped (\subData1 -> (subData1^._2) ) listofstudent) ) 
           

        namesListBysub =  (nub(over mapped (\nameData -> (nameData^._2) ) subjectStudentList))
        --nameData
        in DL.zip (subjectListbynames) namesListBysub 


--------------------------------------------------



pad :: Int -> String -> String
pad num xs = " " DL.++ xs DL.++ (DL.replicate (num-(DL.length xs) ) ' ')

forDuplicates :: Score -> ValidSubjects -> IO()
forDuplicates score vsubject = Prelude.putStrLn (DL.foldl' (\arr name -> arr DL.++ name DL.++ "\n") "" (duplicateStudents score vsubject))

forAverage :: Score -> ValidSubjects -> IO()
forAverage score vsubject=
    Prelude.putStrLn "Enter the subject name" >> Prelude.getLine >>= \subjectname -> 
                                                                    if(subjectname `DL.elem` vsubject )
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


{-}
forStudentsListForExam1 :: Score -> ValidSubjects -> IO()
forStudentsListForExam1 score vsubject = 
    Prelude.putStrLn (DL.foldl' (\arr1 (subjectname,list)-> arr1 DL.++ "\n" DL.++ pad 20 subjectname DL.++ " | " DL.++ 
            DL.foldl' (\arr2 (student)-> arr2 DL.++ pad 20 student DL.++ " ") "" list )
                     "" (studentsListForExam1 score vsubject))
-}

forStudentsListForExam1 :: Score -> ValidSubjects -> IO()
forStudentsListForExam1 score vsubject = Prelude.putStrLn (DL.foldl' (\opstr (sname,list)-> 
        opstr Prelude.++ "\n" Prelude.++ sname Prelude.++ " | " Prelude.++ DL.foldl' (\str (studname)-> 
            str Prelude.++ studname Prelude.++ " ") "" list
        ) "" (studentsListForExam1 score vsubject))
                  
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






main1 :: BL.ByteString -> DB.ByteString -> IO ()
main1 myscorefile mysubjectfile = do 
    Prelude.putStrLn "Enter \n 1. Average \n 2. Duplicate \n 3. Std Deviation \n 4. Invalid scores \n 5. Students for exam \n 6. Exit"
    choice <- Prelude.getLine 
    let fscore =  scoreRecord1 (myscorefile)
        fsubject =  formatSubjects (toS mysubjectfile)
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
                        let scorelist = over mapped (\(name1,subject1,marks1) -> [name1,subject1,marks1] ) score1
                            grouping1 = DL.groupBy  (\a  x ->  ([a^.._head]) ==  ([x^.._head]) ) (DV.toList scorelist) 
                         in Prelude.mapM (\ records -> function1 records ("",[]) ) grouping1
   
                         
--over (mapped._2) (+1) [(1,2),(1,3),(1,4)]
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
                                                Just mark1 -> if name==""
                                                                then function1 (DL.tail records) (sname,[(ssubject,mark1::Int )])
                                                               else function1 (DL.tail records) (name,score1 DL.++ [(ssubject,mark1 ::Int)])
                                                Nothing -> Left "invalid marks"
                                                _ -> Left "Invalid"