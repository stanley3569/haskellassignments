module Chapter2.GenExam4 where
import Data.List
import Data.Char


score :: [(String,[(String,Int)])]
score=[ ("Mojo", [("English", 84), ("Chemistry", 81), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)])
    , ("Captain Jack", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Dexter", [("Tamil", 110), ("Biology", 100), ("Geography", 100), ("Physics", -55)]) -- Note that marks in "Tamil" are greater than 100
    ]

validsubject :: [String]
validsubject = ["English", "Geography", "Physics", "Chemistry", "Economics", "Computer Science"]

type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)                                                                                                   --type Validate = Bool            --(Bool,String)                   --changed for 4


validateMarksSubjects :: ([(StudentName,[(Subject,Marks,Bool,String)])] -> a) -> InputScore -> ValidSubjects -> a
validateMarksSubjects testfunc score validsubject = 
    let dupicatenames = (duplicateStudents score)
        newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = map (\(studentName,scorenames)->(studentName,              --1.0
            concatMap(\(subjectname,marks) ->
            if (marks < 0)
                then [(subjectname,marks,False,"negative marks")]
            else if (marks > 100)
                then [(subjectname,marks,False,"marks higher than 100")]
            else if(subjectname `elem` validsubject)
                then [(subjectname,marks,True,"valid Subject")]
            else [(subjectname,marks,False,"unknown Subject")]  ) scorenames )) newscore
        validMarksList = testfunc validMarks
       in validMarksList



averageg  :: InputScore -> ValidSubjects -> Subject-> Float 
averageg score validsubject subjectName =
    averageMarks  (\(x,y) -> ( fromIntegral( sum( x) ) / fromIntegral(y))) score validsubject subjectName 




averageMarks :: (([Marks],Int) -> a) -> InputScore -> ValidSubjects -> Subject -> a
averageMarks testfunc1 score validsubject subjectName =
    let subjectinside = concat $ validateMarksSubjects (\x -> map(\(a,b) -> b) x) score validsubject
        valuesAverage = foldl' (\(x,y) (subjectname,marks,validation,validateString) -> 
                        if(subjectname==subjectName && validation == True)
                            then (x++[marks],length (x++[marks]))
                        else (x,y)
                            )   ([],0) subjectinside
        averagem = testfunc1 valuesAverage                                                                      --score validsubject subjectName
     in averagem


type DuplicateStudentList =  [String]                                                                                         
duplicateStudents :: InputScore -> DuplicateStudentList
duplicateStudents score = 
    let studentnames = map (\ x  ->  fst x ) score
        duplicatenames = concatMap(\x -> if(length x > 1) then [head x] else [] ) (group(sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        in duplicatenames


standardDeviation :: InputScore -> ValidSubjects -> Subject -> Float
standardDeviation score validsubject subjectName = 
    let averagevalue = averageg score validsubject subjectName                                --averageMarks (\x ->(fromIntegral (sum x) / fromIntegral (length x))) score validsubject subjectName
        listsub =fst $ averageMarks (\marks ->  marks ) score validsubject subjectName
        calculateXM = ( zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = sum (map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (length(listsub) ) )   )



invalidScores :: InputScore -> ValidSubjects -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score validsubject =                    
    let invalidData4 = validateMarksSubjects (\x -> (map (\(studentname,scorelist) -> 
            (studentname,(filter (\(subjectname,marks,validation,validateString) -> 
             (validation == False)) scorelist)))) x  ) score validsubject
        
        invalidName = map(\(studentname,subjectinfo) -> (studentname,map (\(subjectname,marks,validation,validateString)
                        ->(subjectname,marks,validateString)) subjectinfo)) invalidData4
     in invalidName


-----------
