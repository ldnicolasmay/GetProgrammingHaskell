-- ch04.hs

import Data.List

ifEvenInc :: (Integral a) => a -> a
-- ifEvenInc n = if even n then n + 1 else n
-- ifEvenInc n = ifEven inc n
ifEvenInc = ifEven inc  -- pfs

ifEvenDouble :: (Integral a) => a -> a
-- ifEvenDouble n = if even n then n * 2 else n
ifEvenDouble = ifEven double

ifEvenSquare :: (Integral a) => a -> a
-- ifEvenSquare n = if even n then n ^ 2 else n
ifEvenSquare = ifEven square

ifEvenCube :: (Integral a) => a -> a
ifEvenCube = ifEven cube

ifEvenNegate :: (Integral a) => a -> a
ifEvenNegate = ifEven negate 

ifEven :: (Integral a) => (a -> a) -> a -> a
ifEven f n = if even n then f n else n

inc :: (Num a) => a -> a
inc n = n + 1

double :: (Num a) => a -> a
double n = n * 2

square :: (Num a) => a -> a
square n = n ^ 2

cube :: (Num a) => a -> a
cube n = n ^ 3

author = ("Will","Kurt")

names = [ ("Ian","Curtis")
        , ("Bernard","Sumber")
        , ("Peter","Hook")
        , ("Stephen","Morris")
        , ("Bill","Morris")
        ]

-- λ> sortBy (\a b -> snd a `compare` snd b) names
-- [("Ian","Curtis"),("Peter","Hook"),("Stephen","Morris"),("Bernard","Sumber")]

-- compareLastNames :: String -> String -> Ordering
compareLastNames :: (Ord c) => (a,c) -> (a,c) -> Ordering
-- compareLastNames name1 name2 =
--   if lastName1 > lastName2
--   then GT
--   else 
--     if lastName1 < lastName2
--     then LT
--     else EQ
--   where lastName1 = snd name1
--         lastName2 = snd name2
-- -- --
-- compareLastNames name1 name2
--   | lastName1 > lastName2  = GT
--   | lastName1 < lastName2 = LT
--   | otherwise = EQ
--   where lastName1 = snd name1
--         lastName2 = snd name2
-- -- --
compareLastNames name1 name2 = lastName1 `compare` lastName2
  where lastName1 = snd name1
        lastName2 = snd name2


compareLastNames' :: (Ord a) => (a,a) -> (a,a) -> Ordering
compareLastNames' name1 name2
  | lastName1 > lastName2  = GT
  | lastName1 < lastName2 = LT
  | lastName1 == lastName2 && firstName1 > firstName2 = GT
  | lastName1 == lastName2 && firstName1 < firstName2 = LT
  | otherwise = EQ
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2

-- λ> sortBy compareLastNames names
-- [("Ian","Curtis"),("Peter","Hook"),("Stephen","Morris"),("Bill","Morris"),("Bernard","Sumber")]
-- λ> sortBy compareLastNames' names
-- [("Ian","Curtis"),("Peter","Hook"),("Bill","Morris"),("Stephen","Morris"),("Bernard","Sumber")]


addressLetter :: (String, String) -> String -> String
addressLetter name location = 
  nameText ++ " - " ++ location
  where nameText = fst name ++ " " ++ snd name

sfOffice name = 
  if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = nameText ++ ", Esq - PO Box 666 - Washington, DC 20405"
  where nameText = fst name ++ " " ++ snd name

getLocationFxn :: String -> ((String,String) -> String)
getLocationFxn location =
  case location of
    "dc"   -> dcOffice
    "ny"   -> nyOffice
    "reno" -> renoOffice
    "sf"   -> sfOffice
    _      -> \name -> fst name ++ " " ++ snd name

addressLetter' :: (String,String) -> String -> String
-- addressLetter' name location = locationFxn name
--   where locationFxn = getLocationFxn location
addressLetter' name location = (getLocationFxn location) name


