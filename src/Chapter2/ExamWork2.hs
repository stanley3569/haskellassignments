module Chapter2.ExamWork2 where
import Data.List
import Data.Char


score :: [(String,[(String,Int)])]
score=[ ("Mojo", [("English", 84), ("Chemistry", 80), ("Physics", 95), ("Geography", 75)])
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
type Average = Float

validateMarksSubjects :: InputScore -> ValidSubjects -> StudentName -> Subject -> Validate
validateMarksSubjects score validsubject studentName subjectName = 
      let validMarks = snd (head (filter (\x -> (fst x == subjectName)) (head (map (snd) (filter(\x -> (fst x) == studentName) score) ) )   ))              --1.0
      in  if (validMarks < 0)
                then (False,"negative marks")
          else if (validMarks > 100)
                then (False,"marks higher than 100")
          else if(subjectName `elem` validsubject)
                then (True,"valid Subject")
          else (False,"unknown Subject")



type DuplicateStudentList =   [String]                                        --3rd                              ["John Doe","John Doe"]                                       --3rd point          
duplicateStudents :: InputScore -> DuplicateStudentList
duplicateStudents score = 
   nub $ concat( filter (\x -> (length(x)>1 ) )  ( groupBy(\x y-> x==y ) (sort (fst (unzip(score))) ) )  ) 



averageMarks :: ([Int]-> Float)->InputScore ->ValidSubjects -> Subject -> Float             -- SubjectAverage             --1st
averageMarks test score validsubject subject =
 let listSubMarks = snd (unzip(filter (\x -> (fst x == subject)) (concatMap (\x -> (snd x)) (allValidNames score validsubject) ) ) )
     average = test listSubMarks
   in average
 
{-}
test1 score validsubject subject =
        snd (unzip(filter (\x -> (fst x == subject)) (concatMap (\x -> (snd x)) (allValidNames score validsubject) ) ) )
-}


standardDeviation :: InputScore -> ValidSubjects -> Subject -> Float          --2nd                           
standardDeviation score validsubject subjectName = 
    let averagevalue = generalizeAverage score validsubject subjectName                                         --averageMarks (\x -> fromIntegral( fromIntegral( sum((( x))) ) `div` fromIntegral(length((( x)))  )   ) )  score validsubject subjectName                         --snd(averageMarks score validsubject subjectName)
        listsub = (filter (\x -> (fst x == subjectName)) (concat ( snd (unzip(allValidSubjects score validsubject) ) ) ))
        markslist = (snd(unzip listsub))                                                      
        calculateXM = ( zipWith (\x y -> x-y)  markslist [round(averagevalue)..] )
        calculateXMSquare = sum (map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` length(markslist) ) )

generalizeAverage  :: InputScore -> ValidSubjects -> Subject-> Float 
generalizeAverage score validsubject subjectName =averageMarks  (\x -> fromIntegral( fromIntegral( sum((( x))) ) `div` fromIntegral(length((( x)))  )   ) ) score validsubject subjectName 
    

    


allValidSubjects :: InputScore -> ValidSubjects -> InputScore                                               --gives valid result
allValidSubjects score validsubject = 
    map(\x -> (fst x, (validSubjects score validsubject (fst x)))) score


--
type SubjectMarks = [(String,Int)]                    
validSubjects :: InputScore -> ValidSubjects -> StudentName -> SubjectMarks                           --[(subject,marks)] ---give students valid subject and marks
validSubjects score validsubject studentname = 
    concatMap (\x -> 
        if((fst (validateMarksSubjects score validsubject studentname (fst x)))) 
            then [(fst x,snd x)] 
        else []  ) (head (map (snd) (filter(\x -> (fst x) == studentname) score) ) )



type ValidStudents = [(String,[(String,Int)])] 
allValidNames :: InputScore -> ValidSubjects -> ValidStudents                                                --gives result of student with valid name,sub,marks
allValidNames score validsubject = 
    let validStudentlist = map(\x -> (fst x, (validSubjects score validsubject (fst x)))) score
    in filter (\x -> ((fst x) `notElem` (duplicateStudents validStudentlist))) validStudentlist


invalidSubjects :: InputScore -> ValidSubjects -> StudentName -> [(String,Int,String)]                                  --gives sub,marks,unknow sub
invalidSubjects score validsubject studentName =                                                                            ---[("Tamil",110,"marks higher than 100"),("Biology",100,"unknown Subject"),("Physics",-55,"negative marks")]
    concatMap (\x -> if( (fst (validateMarksSubjects  score validsubject studentName (fst x)))==False )
                        then  [(fst x,snd x,snd (validateMarksSubjects score [] studentName (fst x)))] 
                     else [] ) (snd (head (filter (\x -> (fst x)== studentName) score)))

{-}
test111 score validsubject studentName = 
    --(snd (head (filter (\x -> (fst x)== studentName) score)))
    concatMap(\(x,y)-> if(x==studentName)
                    then y
                 else [] ) score
-}

type StudentsForInvalidScoreSubject = [(String, [(String,Int,String)] )]                                                            --4th point
invalidSubjectMarksStudent :: InputScore -> ValidSubjects ->  StudentsForInvalidScoreSubject                        --4         [("Mojo",[("Chemisty",80,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Captain Jack",[("Chemisty",66,"unknown Subject"),("Phsyics",33,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Bahubali",[("Hindi",45,"unknown Subject"),("Biology",-90,"negative marks"),("Geography",-75,"negative marks")]),("Dexter",[("Tamil",110,"marks higher than 100"),("Biology",100,"unknown Subject")])]                                   --[(String, [(String,Int,String)] )]
invalidSubjectMarksStudent score validsubject = 
    concatMap(\x -> [(fst x,(invalidSubjects score validsubject (fst x)))] ) score

---



studentForExam :: InputScore -> ValidSubjects -> StudentName -> Subject -> Bool                            --validates if a student is ther in the course                                     
studentForExam score validsubject studentName subjectName =
     (filter (\x -> (fst x)==subjectName) (validSubjects score validsubject studentName)) /= []



allStudentsForExam :: InputScore -> ValidSubjects -> Subject -> (Subject,[StudentName])                           --gives subject with valid students
allStudentsForExam score validsubject subjectName = 
    let subjectNamesOfStudents =                                                                                --("Geography",["Mojo","Captain Jack","Dexter"])
                concatMap(\x -> 
                    if (studentForExam score validsubject (fst x) subjectName)
                        then [(subjectName,[fst x])]
                    else [] )     (allValidNames score validsubject)
        studentNamesss =   (subjectName,concat(snd(unzip (subjectNamesOfStudents)  ) ) ) 
        in studentNamesss    


type StudentForSubject = [(String,[String])]                                                                                                --5th point
studentsListForExam :: ValidStudents -> ValidSubjects -> StudentForSubject                        -----5                [("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",[]),("Economics",[]),("Computer Science",[])]
studentsListForExam score validsubject= 
    concatMap (\ x -> [allStudentsForExam score validsubject x])  validsubject


---

type GroupingStudentSubject = [([String],[String])]                                       ---6th point
grouping :: InputScore -> ValidSubjects -> GroupingStudentSubject
grouping score validsubject = 
                let listofstudent = map(\(x,y)-> (x,fst(unzip y)))    (allValidNames score validsubject)         --studentDetails score validsubject                                               --1--[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]  
                    subjectStudentList = studentsListForExam score validsubject                                     --2--[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]
                    subjectListbynames = delete [] (nub(map (\(y,z)-> z) listofstudent) )                           --4 --3-using1- [["English","Chemistry","Physics","Geography"],["Chemistry","Geography"],["Geography"]]                    
                    namesListBysub =  (nub(map(\(y,z)-> z) subjectStudentList))                                     --4--4-using2--[["Mojo"],["Mojo","Captain Jack","Dexter"],["Mojo","Captain Jack"],[]]                     
                in zip (subjectListbynames) namesListBysub                                                          --4
                                                             
     
{-}
type StudentDetails = [(String,[String])]
studentDetails :: InputScore -> ValidSubjects -> StudentDetails 
studentDetails score validsubject = 
    --map(\x -> ( fst(x),head[( fst ( unzip (snd(x)) ) )] ) ) (allValidNames score validsubject)                                              --[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]                                                  
    map(\(x,y)  -> (x,fst(unzip y)))    (allValidNames score validsubject) 
-}