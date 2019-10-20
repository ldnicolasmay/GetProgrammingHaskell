-- lesson12.hs


-- 12.1

-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ " in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int


type PatientName = (String, String)

firstName :: PatientName -> FirstName
firstName = fst

lastName :: PatientName -> LastName
lastName = snd

testPatient = ("John", "Doe")

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' (fName, lName) age height = name ++ " " ++ ageHeight
 where
  name      = lName ++ ", " ++ fName
  ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ " in.)"



-- 12.2

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"


data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

-- data Waist  = Skinny | Regular | Fat
-- data Inseam = Short | Medium | Long
-- data PantSize = PantSize Waist Inseam

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

myBloodType1 = BloodType O Pos
myBloodType2 = BloodType AB Neg
myBloodType3 = BloodType B Pos
btANeg = BloodType A Neg
btBPos = BloodType B Pos
btABNeg = BloodType AB Neg
btOPos = BloodType O Pos

-- A can donate to A and AB.
-- B can donate to B and AB.
-- AB can donate only to AB.
-- O can donate to anybody.

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O  _) _                = True
canDonateTo (BloodType A  _) (BloodType A  _) = True
canDonateTo (BloodType A  _) (BloodType AB _) = True
canDonateTo (BloodType B  _) (BloodType B  _) = True
canDonateTo (BloodType B  _) (BloodType AB _) = True
canDonateTo (BloodType AB _) (BloodType AB _) = True
canDonateTo _                _                = False


-- type FirstName = String -- from above
-- type LastName  = String -- from above
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l            ) = l ++ ", " ++ f
showName (NameWithMiddle f m l) = l ++ ", " ++ f ++ " " ++ m

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"



-- 12.3

-- data for new `Patient` type
-- Label       | Type
-- ------------|--------
-- Name        | Name
-- Sex         | Sex
-- Age (yr)    | Int
-- Height (in) | Int
-- Weight (lb) | Int
-- Blood type  | BloodType

-- use `data` keyword to create the new `Patient` type  
-- data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSueDoe :: Patient
janeSueDoe =
  Patient (NameWithMiddle "Jane" "Sue" "Doe") Female 28 68 135 (BloodType O Neg)

-- use data keyword w/ record syntax `{ ... }` to create new `Patient` type
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType
                       }

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg
                      }

jackieSmithUpdated = jackieSmith { age = 44 }



-- Q12.1

canDonateTo' :: Patient -> Patient -> Bool
canDonateTo' p1 p2 = canDonateTo (bloodType p1) (bloodType p2)


-- Q12.2

-- patientSummary :: Patient -> String
patientSummary p = 
  putStrLn ( stars ++ 
             nameSumm p ++ 
             sexSumm p ++
             ageSumm p ++
             heightSumm p ++
             weightSumm p ++
             bloodTypeSumm p ++
             stars
           )

stars :: String
stars = "**************\n"

nameSumm :: Patient -> String
nameSumm p = "Patient Name: " ++ showName (name p) ++ "\n"

sexSumm :: Patient -> String
sexSumm p = "Sex: " ++ showSex (sex p) ++ "\n"

ageSumm :: Patient -> String
ageSumm p = "Age: " ++ show (age p) ++ "\n"

heightSumm :: Patient -> String
heightSumm p = "Height: " ++ show (height p) ++ "\n"

weightSumm :: Patient -> String
weightSumm p = "Weight: " ++ show (weight p) ++ "\n"

bloodTypeSumm :: Patient -> String
bloodTypeSumm p = "Blood Type: " ++ showBloodType (bloodType p) ++ "\n"

