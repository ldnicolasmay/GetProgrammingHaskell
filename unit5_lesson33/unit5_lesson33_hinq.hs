-- unit5_lesson33_hinq.hs
-- Capstone: SQL-like Queries in Haskell


import           Control.Applicative
import           Control.Monad


-- 33.1 Getting Started

data Name = Name { firstName :: String
                 , lastName  :: String
                 }

instance Show Name where
  show (Name first last) = mconcat [first," ",last]

data GradeLevel = Freshman
                | Sophomore
                | Junior
                | Senior
                deriving (Read,Show,Eq,Ord,Enum)

data Student = Student { studentId   :: Int
                       , gradeLevel  :: GradeLevel
                       , studentName :: Name
                       } deriving (Show)

students :: [Student]
students = [ (Student 1 Senior    (Name "Audre" "Lorde"))
           , (Student 2 Junior    (Name "Leslie" "Silo"))
           , (Student 3 Freshman  (Name "Judith" "Butler"))
           , (Student 4 Senior    (Name "Guy" "Debord"))
           , (Student 5 Sophomore (Name "Jean" "Baudrillard"))
           , (Student 6 Junior    (Name "Julia" "Kristeva"))
           ]


-- 33.2 Basic Queries for Your List: Select and Where

-- SELECT statement in SQL:
-- SELECT studentName FROM students;

-- SELECT + WHERE statement in SQL:
-- SELECT * FROM students WHERE gradeLevel = 'Senior';


-- _select :: (a -> b) -> [a] -> [b]  -- redefined to work with Monads below
_select prop vals = do
  val <- vals
  return (prop val)

-- λ> _select studentId students
-- [1,2,3,4,5,6]

-- λ> _select gradeLevel students
-- [Senior,Junior,Freshman,Senior,Sophomore,Junior]

-- λ> _select studentName students
-- [Audre Lorde,Leslie Silo,Judith Butler,Guy Debord,Jean Baudrillard,Julia Kristeva]

-- λ> _select (firstName . studentName) students
-- ["Audre","Leslie","Judith","Guy","Jean","Julia"]

-- λ> _select (\x -> (studentId x, gradeLevel x)) students
-- [(1,Senior),(2,Junior),(3,Freshman),(4,Senior),(5,Sophomore),(6,Junior)]

-- λ> _select (\x -> (studentId x, (lastName . studentName) x)) students
-- [(1,"Lorde"),(2,"Silo"),(3,"Butler"),(4,"Debord"),(5,"Baudrillard"),(6,"Kristeva")]


-- _where :: (a -> Bool) -> [a] -> [a]  -- redefined to work with Monads below
_where test vals = do
  val <- vals
  guard (test val) -- `guard` requires `import Control.Monad`
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)


-- 33.3 Joining Course and Teacher Data Types

data Teacher = Teacher { teacherId   :: Int
                       , teacherName :: Name
                       } deriving (Show)

teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "De Beauvoir")
           , Teacher 200 (Name "Susan" "Sontag")
           ]

data Course = Course { courseId    :: Int
                     , courseTitle :: String
                     , teacher     :: Int
                     } deriving (Show)

courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 201 "English" 200
          ]

-- INNER JOIN statement in SQL:
-- SELECT * FROM
--   teachers
-- INNER JOIN
--   courses
--   ON (teachers.teacherId = courses.teacher);

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
-- above function header redefined to use Monads below
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1,d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs

-- λ> _join teachers courses teacherId teacher
-- [ (Teacher {teacherID = 100, teacherName = Simone De Beauvoir},
--    Course {courseId = 101, courseTitle = "French", teacher = 100}),
--   (Teacher {teacherID = 200, teacherName = Susan Sontag},
--    Course {courseId = 201, courseTitle = "English", teacher = 200}) ]


-- 33.4 Building Your HINQ Interface and Example Queries

joinData     = _join teachers courses teacherId teacher
whereResult  = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery =
  (\joinData ->
     (\whereResult ->
        selectQuery whereResult
     ) (whereQuery joinData)
  ) joinQuery

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))


-- 33.5 Making a HINQ Type for Your Queries

_select :: Monad m => (a -> b) -> m a -> m b
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)

data HINQ m a b = HINQ  (m a -> m b)  (m a)  (m a -> m a)
                | HINQ_ (m a -> m b)  (m a)  -- _where removed
                --      _select       _join  _where

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ  sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause)         = _hinq sClause jClause (_where (\_ -> True))


-- 33.6 Running Your HINQ Queries

listQuery1 :: HINQ [] (Teacher,Course) Name
listQuery1 = HINQ (_select (teacherName . fst))
                  (_join teachers courses teacherId teacher)
                  (_where ((=="English") . courseTitle . snd))


listQuery2 :: HINQ [] Teacher Name
listQuery2 = HINQ_ (_select teacherName)
                   (teachers)

listQuery3 :: HINQ [] Teacher Teacher
listQuery3 = HINQ_ (_select (\x -> Teacher (teacherId x) (teacherName x)))
                   (teachers)

listQuery4 :: HINQ [] (Teacher,Course) Teacher
listQuery4 = HINQ_ (_select (\(t,c) -> Teacher (teacherId t) (teacherName t)))
                   (_join teachers courses teacherId teacher)

listQuery5 :: HINQ [] (Teacher,Course) (Teacher,Course)
listQuery5 = HINQ_ (_select (\(t,c) -> (Teacher (teacherId t) (teacherName t),
                                        Course (courseId c) (courseTitle c) (teacher c))))
                   (_join teachers courses teacherId teacher)

listQuery6 :: HINQ [] (Teacher,Course) (Teacher,Course)
listQuery6 = HINQ (_select (\(t,c) -> (Teacher (teacherId t) (teacherName t),
                                       Course (courseId c) (courseTitle c) (teacher c))))
                  (_join teachers courses teacherId teacher)
                  (_where (\(_,c) -> "French" == courseTitle c))


possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher,Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher,Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

maybeQuery2a :: HINQ Maybe Teacher Name
maybeQuery2a = HINQ_ (_select teacherName)
                    (possibleTeacher)

maybeQuery3 :: HINQ Maybe Teacher Teacher
maybeQuery3 = HINQ_ (_select (\x -> Teacher (teacherId x) (teacherName x)))
                    (possibleTeacher)


data Enrollment = Enrollment { student :: Int
                             , course  :: Int
                             } deriving (Show)

enrollments :: [Enrollment]
enrollments = [ (Enrollment 1 101)
              , (Enrollment 2 101)
              , (Enrollment 2 201)
              , (Enrollment 3 101)
              , (Enrollment 4 101)
              , (Enrollment 4 201)
              , (Enrollment 5 101)
              , (Enrollment 6 201)
              ]

studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name,Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments courses snd courseId)
                        (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ


getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where courseQuery = HINQ (_select (fst . fst))
                           (_join studentEnrollments courses snd courseId)
                           (_where ((== courseName) . courseTitle . snd))


