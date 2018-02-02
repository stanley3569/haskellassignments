module Chapter3.GenExam5 where
import Data.List
import Data.Char

data StudentName = MkName String deriving (Eq,Show,Ord)
data Subject = MkSubject String deriving (Eq,Show,Ord)
data Marks = MkMarks Int deriving (Eq,Show,Ord)
data SubjectList = MkSubjectList [Subject] deriving (Eq, Show, Ord)

data Validate = MkValidate Bool deriving (Eq, Show, Ord)


type InputScore = [(StudentName,[(Subject,Marks)])]

score :: InputScore
score=[ (MkName "Mojo", [(MkSubject"English", MkMarks 84), (MkSubject"Chemistry",MkMarks 81), (MkSubject"Physics",MkMarks 95), (MkSubject"Geography",MkMarks 75)])
    , (MkName "John Doe", [(MkSubject"Chemistry",MkMarks 80), (MkSubject"Physics",MkMarks 95), (MkSubject"Geography",MkMarks 75)])
    , (MkName "Captain Jack", [(MkSubject"Chemistry",MkMarks 66), (MkSubject"Phsyics",MkMarks 33), (MkSubject"Geography",MkMarks 56)]) -- Note the spelling error in "Physics"
    , (MkName "John Doe", [(MkSubject"Chemistry",MkMarks 90), (MkSubject"Economics",MkMarks 45), (MkSubject"Geography",MkMarks 56)]) -- Note that "John Doe" is a repeat entry
    , (MkName "Bahubali", [(MkSubject"Hindi",MkMarks 45), (MkSubject"Biology",MkMarks (-90) ), (MkSubject"Geography",MkMarks (-75))]) -- Note the negative marks in "Biology" & "Geography"
    , (MkName "Dexter", [(MkSubject"Tamil",MkMarks 110), (MkSubject"Biology",MkMarks 100), (MkSubject"Geography",MkMarks 100), (MkSubject"Physics",MkMarks (-55))]) -- Note that marks in "Tamil" are greater than 100
    ]



validsubject :: [Subject]
validsubject = [MkSubject"English",MkSubject "Geography", MkSubject "Physics",MkSubject "Chemistry",MkSubject "Economics",MkSubject "Computer Science"]

{-}
type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)         
-}

validateMarks :: InputScore -> [(StudentName, [(Subject, Marks, Validate, String)])]
validateMarks score =
    let validmarks = map (\(studentName,scorenames)->(studentName,             
                        concatMap(\(subjectname,MkMarks marks) ->
                            if (marks < 0)
                                then [(subjectname,MkMarks marks,MkValidate False,"negative marks")]
                            else if (marks > 100)
                                then [(subjectname,MkMarks marks,MkValidate False,"marks higher than 100")]
                            else if(subjectname `elem` validsubject)
                                then [(subjectname,MkMarks marks,MkValidate True,"valid Subject")]
                            else [(subjectname,MkMarks marks,MkValidate False,"unknown Subject")]  ) scorenames )) score
        in validmarks



validateMarksSubjects ::(b->  (StudentName,Bool,[(Subject,Marks,Validate,String)]) -> b ) ->b -> InputScore -> b
validateMarksSubjects testfunc accumulator score= 
    let dupicatenames = (duplicateStudents score)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = validateMarks score
        newscore =  map(\(studentname,subjectinfo) -> 
                            if (studentname `elem` dupicatenames) 
                                then (studentname,False,subjectinfo) 
                            else (studentname,True,subjectinfo)) validMarks
    in foldl' (\x y -> testfunc x y) accumulator newscore 


                                                                                       
duplicateStudents :: InputScore -> [StudentName]
duplicateStudents score = 
    let studentnames = map (\ x  ->  fst x ) score
        duplicatenames = concatMap(\x -> if(length x > 1) then [head x] else [] ) (group(sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        in duplicatenames


invalidScores :: InputScore  -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score =  
    let invalidData3 = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->                         
                               newlist++[(studentname, (concatMap (\(subjectname,marks,MkValidate validsubject,validmessage)->
                                        if(validsubject==False)
                                            then [(subjectname,marks,validmessage)]
                                        else [] )) subjectinfo)] ) [] score 
        in invalidData3

averageMarks :: InputScore -> Subject -> Float
averageMarks score subjectName =
    let markslist = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                                newlist++(concatMap (\(subjectname,MkMarks marks,MkValidate validsubject,validmessage)->
                                    if(validstudent==True && subjectname==subjectName && validsubject == True)
                                        then [marks]
                                    else [] )) subjectinfo
                                ) [] score 
        averageSubject =  ( fromIntegral( sum( markslist) ) / fromIntegral(length markslist))                                          
     in averageSubject



standardDeviation :: InputScore -> Subject -> Float
standardDeviation score subjectName = 
    let averagevalue = averageMarks score subjectName                               
        listsub = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,MkMarks marks,MkValidate validsubject,validmessage)->
                            if(validstudent==True && subjectname==subjectName && validsubject == True)
                                then [marks]
                            else [] )) subjectinfo
            ) [] score 
        calculateXM = ( zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = sum (map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (length(listsub) ) )   )  


------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [(MkSubject "English", []),(MkSubject "Geography", []),(MkSubject "Physics", []),(MkSubject "Chemistry", []),(MkSubject "Economics", []),(MkSubject "Computer Science", [])]



-------------------------------------------

studentsListForExam1 :: InputScore -> [(Subject, [StudentName])]
studentsListForExam1 score = 
    let listsub =validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,marks,MkValidate validsubject,validmessage)->
                            if(validstudent==True && validsubject == True)
                                then [(subjectname,studentname)]
                            else [] )) subjectinfo
            ) [] score 
        studentlist = map (\z -> foldl' (\ (subjectname,studentname) (x,y) ->
                                            if(x == subjectname)
                                                then (subjectname,studentname++[y])
                                            else (subjectname,studentname)   )
                                        z listsub) validSubjectsStudentList
    in studentlist




grouping :: InputScore -> [([Subject], [StudentName])]
grouping score  = 
    let listofstudent = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        if(validstudent==True)
                            then newlist++[(studentname,concatMap(\(subjectname,marks,MkValidate validsubject,validmessage) -> 
                                    if validsubject
                                        then [subjectname]
                                    else []
                                    ) subjectinfo)]
                        else newlist
                        ) [] score

        subjectStudentList = studentsListForExam1 score 
        subjectListbynames = delete [] (nub(map (\(y,z)-> z) listofstudent) )                           
        namesListBysub =  (nub(map(\(y,z)-> z) subjectStudentList))
        in zip (subjectListbynames) namesListBysub  
