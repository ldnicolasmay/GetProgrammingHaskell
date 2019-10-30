-- unit5_lesson31_candidate.hs


import qualified Data.Map as Map


data Grade = F | D | C | B | A deriving (Eq,Ord,Enum,Show,Read)

data Degree = HS | BA | MS | PhD deriving (Eq,Ord,Enum,Show,Read)

data Candidate = Candidate { candidateId :: Int
                           , codeReview  :: Grade
                           , cultureFit  :: Grade
                           , education   :: Degree
                           } deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (==True) tests
  where passedCoding  = codeReview candidate > B
        passedCulture = cultureFit candidate > C
        educationMin  = education candidate >= MS
        tests         = [ passedCoding
                        , passedCulture
                        , educationMin
                        ]

cand1 :: Candidate
cand1 = Candidate { candidateId = 1
                  , codeReview  = A
                  , cultureFit  = B
                  , education   = MS
                  }

cand2 :: Candidate
cand2 = Candidate { candidateId = 2
                  , codeReview  = A
                  , cultureFit  = A
                  , education   = BA
                  }


readInt :: IO Int
readInt = getLine >>= (return . read)
-- readInt = getLine >>= \input -> return (read input :: Int)
-- readInt = getLine >>= \input -> (\int -> return int) (read input :: Int)
-- readInt = do
--   input <- getLine
--   let int = read input :: Int
--   return int

readGrade :: IO Grade
readGrade = getLine >>= (return . read)
-- readGrade = getLine >>= \input -> return (read input :: Grade)
-- readGrade = getLine >>= \input -> (\grade -> return grade) (read input :: Grade)
-- readGrade = do
--   input <- getLine
--   let grade = read input :: Grade
--   return grade

readDegree :: IO Degree
readDegree = getLine >>= (return . read)
-- readDegree = getLine >>= \input -> return (read input :: Degree)
-- readDegree = getLine >>= \input -> (\degree -> return degree) (read input :: Degree)
-- readDegree = do
--   input <- getLine
--   let degree = read input :: Degree
--   return degree


readCandidate :: IO Candidate
readCandidate = do
  --
  putStrLn "Enter ID:"
  cId <- readInt
  --
  putStrLn "Enter code grade:"
  codeGrade <- readGrade
  --
  putStrLn "Enter culture fit grade:"
  cultureGrade <- readGrade
  --
  putStrLn "Enter education:"
  degree <- readDegree
  --
  return Candidate { candidateId = cId
                   , codeReview  = codeGrade
                   , cultureFit  = cultureGrade
                   , education   = degree
                   }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate    <- readCandidate
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return ("\nCandidate " <> statement <> ".\n")

-- main :: IO ()
-- main = assessCandidateIO >>= putStrLn


candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview  = A
                       , cultureFit  = A
                       , education   = BA
                       }
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview  = C
                       , cultureFit  = A
                       , education   = PhD
                       }
candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview  = A
                       , cultureFit  = B
                       , education   = MS
                       }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1,candidate1)
                           , (2,candidate2)
                           , (3,candidate3)
                           ]


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate    <- Map.lookup cId candidateDB  -- candidate :: Candidate
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return ("Candidate " <> statement <> ".")


-- foo :: Maybe String -> String
-- foo (Just x) = x
-- foo Nothing  = "Error: Id not found"


candidates :: [Candidate]
candidates = [ candidate1
             , candidate2
             , candidate3
             ]


assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate    <- candidates
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return ("Candidate " <> statement <> ".")


-- General `assessCandidate` monadic function

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate mcandidate = do
  candidate    <- mcandidate
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return ("Candidate " <> statement <> ".")


-- λ> assessCandidate readCandidate 
-- Enter ID:
-- 1
-- Enter code grade:
-- A
-- Enter culture fit grade:
-- A
-- Enter education:
-- MS
-- "Candidate passed."

-- λ> assessCandidate (Data.Map.lookup 1 candidateDB)
-- Just "Candidate failed."

-- λ> assessCandidate candidates
-- ["Candidate failed.","Candidate failed.","Candidate passed."]


-- Q31.1

-- main :: IO ()
-- main = do
--   putStrLn "What is the size of pizza 1"
--   size1 <- getLine
--   putStrLn "What is the cost of pizza 1"
--   cost1 <- getLine
--   putStrLn "What is the size of pizza 2"
--   size2 <-  getLine
--   putStrLn "What is the cost of pizza 2"
--   cost2 <- getLine
--   let pizza1 = (read size1, read cost1)
--   let pizza2 = (read size2, read cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   putStrLn (describePizza betterPizza)

-- main :: IO ()
-- main = putStrLn "What is the size of pizza 1" >>
--        getLine >>= \size1 ->
--        putStrLn "What is the cost of pizza 1" >>
--        getLine >>= \cost1 ->
--        putStrLn "What is the size of pizza 2" >>
--        getLine >>= \size2 ->
--        putStrLn "What is the cost of pizza 2" >>
--        getLine >>= \cost2 ->
--        (\pizza1 ->
--           (\pizza2 ->
--              (\betterPizza ->
--                 putStrLn (describePizza betterPizaa)
--              ) (comparePizzas pizza1 pizza2)
--           ) (read size2, read cost2)
--        ) (read size1, read cost1)


-- Q31.2

-- listMain :: [Int] -> [Int] -> [Int] -> [Int] -> [String]
-- listMain size1s cost1s size2s cost2s = do
--   size1 <- size1s
--   cost1 <- cost1s
--   size2 <- size2s
--   cost2 <- cost2s
--   let pizza1 = (size1,cost1)
--   let pizza2 = (size2,cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   return (describePizza betterPizza)


-- Q31.3

-- monadMain :: Monad m => m Int -> m Int -> m Int -> m Int -> m String
-- monadMain msize1 mcost1 msize2 mcost2 = do
--   size1 <- msize1
--   cost1 <- mcost1
--   size2 <- msize2
--   cost2 <- mcost2
--   let pizza1 = (size1,cost1)
--   let pizza2 = (size2,cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   return (describePizza betterPizza)

