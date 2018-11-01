module Chapter2.DataFile where

score :: [(String,[(String,Int)])]
score=[ ("Saurabh Nanda", [("English", 84), ("Chemisty", 80), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemisty", 80), ("Physics", 95), ("Geography", 75)])
    , ("Jane Doe", [("Chemisty", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemisty", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)]) -- Note that marks in "Tamil" are greater than 100
    ]

type InputData = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float) 
averageMarks :: InputData -> String -> SubjectAverage
averageMarks = _todo


--checkInvalid :: [(String,[(String,Int)])] -> [(String,[(String,Int,Bool)])]           -- [String]-> String -> Int -> Bool               
--checkInvalid = _todo                                                                                                                           

                                                                                            --type InputData1 = [(String,[(String,Int)])]

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = Bool

validateMarksSubjects ::InputData -> ValidSubjects -> StudentName -> Marks -> Validate                                         --sub,name,marks,valid/invalid
validateMarksSubjects = _todo


                                                                                            --type InputData2 = [(String,[(String,Int)])]


type ValidStudents = [(String,[(String,Int)])] 

validateStudent :: InputData -> ValidStudents                                                      --return only students who are valid
validateStudent = _todo 




                                                                            --type InputData3 = [(String,[(String,Int)])]
type StandardDeviation = Float
type InputSubject = String

calculateStandardDeviation ::InputSubject -> InputData -> StandardDeviation                     --[(String,Int)]
calculateStandardDeviation = _todo




