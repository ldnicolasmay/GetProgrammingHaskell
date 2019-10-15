-- lesson15.hs


-- 15.1 ROT13 cryptography

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

-- rotN :: (Bounded a, Enum a) => Int -> a -> a -- a is the letter to rotate
-- rotN size letter = toEnum ((letterInt + rotateInt) `mod` size)
--   where rotateInt = size `div` 2
--         letterInt = fromEnum letter
-- rotN alphabetSize c = toEnum rotation
--   where halfAlphabet = alphabetSize `div` 2
--         offset = fromEnum c + halfAlphabet
--         rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

-- rotChar :: Char -> Char
-- rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
--   where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterEncode :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncode msg = map rot4l msg
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l     = rotNEncode alphaSize





-- -- --

-- -- My rewrite to ensure I understand:
-- rotChar c = toEnum ((fromEnum c + rotation) `mod` maxBoundChar) :: Char
--   where rotation     = maxBoundChar `div` 2
--         maxBoundChar = 1 + fromEnum (maxBound :: Char)


rotNEncode :: (Bounded a, Enum a) => Int -> a -> a -- a is the letter to rotate
rotNEncode alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize

rotNDecode :: (Bounded a, Enum a) => Int -> a -> a
rotNDecode n c = toEnum rotation
  where halfN    = n `div` 2
        offset   = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
        rotation = offset `mod` n

rotEncode :: String -> String
rotEncode text = map rotCharEncode text
  where alphabetSize  = 1 + fromEnum (maxBound :: Char)
        rotCharEncode = rotNEncode alphabetSize

rotDecode :: String -> String
rotDecode text = map rotCharDecode text
  where alphabetSize  = 1 + fromEnum (maxBound :: Char)
        rotCharDecode = rotNDecode alphabetSize


    
--- 15.2 XOR cryptography

xorBool :: Bool -> Bool -> Bool
xorBool b1 b2 = (b1 || b2) && not (b1 && b2)

-- λ> xorBool <$> [False,True] <*> [False,True]
-- [False,True,True,False]
-- λ> [ b1 `xorBool` b2 | b1 <- [False,True], b2 <- [False,True] ]
-- [False,True,True,False]
-- λ> [False,True] >>= \b1 -> [False,True] >>= \b2 -> return(b1 `xorBool` b2)
-- [False,True,True,False]
-- xorBoolDo :: [Bool]
-- xorBoolDo = do
--   b1 <- [False,True]
--   b2 <- [False,True]
--   return (b1 `xorBool` b2)

-- xorPair :: (Bool,Bool) -> Bool   -- use `zipWith` instead of `map...zip`
-- xorPair (b1,b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor bs1 bs2 = zipWith xorBool bs1 bs2



-- 15.3

type Bits = [Bool]

-- data Bits = FalseB | TrueB deriving (Eq,Ord,Read,Enum,Bounded)
-- instance Show Bits where
--   show False = 0
--   show True  = 1

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
-- intToBits' 0 = [FalseB]
-- intToBits' 1 = [TrueB]
-- intToBits' n = if even n  -- my attempt... same result, less clear
--                then False : intToBits' (n `div` 2)
--                else True  : intToBits' (n `div` 2)
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True  : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal   = n `div` 2
-- intToBits' n = if remainder == 0
--                then FalseB : intToBits' nextVal
--                else TrueB  : intToBits' nextVal
--   where remainder = n `mod` 2
--         nextVal   = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits n = prependFalses ++ reverse reversedBits
  where reversedBits  = intToBits' n
        prependFalses = replicate missingBitCnt False
        missingBitCnt   = maxBits - length reversedBits
        
-- bitsToString :: Bits -> String
-- bitsToString bits = map bitToChar bits
--   where bitToChar b = if b
--                       then '1'
--                       else '0'

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt bits = sum ints
  where ints          = zipWith buildInts bits indexes
        indexes       = [listLen-1,listLen-2 .. 0]
        buildInts b i = if b then 2^i else 0
        listLen       = length bits
-- -- Book does bitsToInt slightly differently; I think mine's objtvly clearer
-- bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
--   where size = length bits
--         indices = [size-1,size-2 .. 0]
--         trueLocations = filter (\x -> fst x == True)
                        -- (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits) -- :: Char

-- ENCODING
-- String -> Char -> Int -> Bits
-- Bits XOR RandomKeyBits -> Encoding
-- DECODING
-- Encoding XOR RandomKeyBits -> Bits
-- Bits -> Int -> Char -> String



-- 15.4 One-Time Pad


myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- xor :: Bits -> Bits -> Bits
-- xor bs1 bs2 = zipWith xorBool bs1 bs2

applyOTP' :: String -> String -> [Bits]
applyOTP' otp msg = zipWith xor otpBitsList msgBitsList
  where otpBitsList = map charToBits otp
        msgBitsList = map charToBits msg

applyOTP :: String -> String -> String
applyOTP otp msg = map bitsToChar xorBitsList
  where xorBitsList = applyOTP' otp msg

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad  


-- 15.5 - Cipher Class

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncode text
  decode Rot text = rotDecode text


data OneTimePad = OTP String
-- newtype OneTimePad = OTP [Char]

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])


-- Extending the exercise

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePrng :: Int -> Int
examplePrng = prng 1337 7 100

--- --- --- 

charPrng :: Int -> Int
charPrng = prng 1337 7 128

--               seed 
buildPrngList :: Int -> [Int]
buildPrngList s = newSeed : buildPrngList newSeed
  where newSeed = charPrng s

data StreamCipher = SC String

instance Cipher StreamCipher where
  encode (SC pad) text = applyOTP pad text
  decode (SC pad) text = applyOTP pad text

mySC :: StreamCipher
mySC = SC (map (bitsToChar . intToBits) (buildPrngList 33))

genStreamCipher :: Int -> StreamCipher
genStreamCipher seed = SC (map (bitsToChar . intToBits) (buildPrngList seed))