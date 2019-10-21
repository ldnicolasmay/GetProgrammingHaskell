-- unit4_lesson25_glitcher.hs


-- 1. Take a filename argument from the user.
-- 2. Read in the binary data for the image file.
-- 3. Randomly alter bytes in the image data.
-- 4. Write a new file containing the glitched image.


import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Control.Monad


-- randomChar :: IO Char
-- randomChar = do
--   randInt     <- randomRIO (0,255)
--   let randChar = toEnum randInt
--   return randChar

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,newChar,after]
  where (before,rest) = BC.splitAt loc bytes
        after         = BC.drop 1 rest
        newChar       = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1,bytesLength)
  charVal  <- randomRIO (0,255)
  return (replaceByte location charVal bytes) 

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
  where (before,rest)  = BC.splitAt start bytes
        (target,after) = BC.splitAt size rest
        changed        = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 5
  let bytesLength = BC.length bytes
  start <- randomRIO (0,bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)


-- main :: IO ()
-- main = do
--   args <- getArgs           -- args :: [String]
--   let fileName = head args  -- fileName :: String
--   --
--   imageFile <- BC.readFile fileName        -- imageFile :: BC.ByteString
--   -- glitched <- return imageFile             -- glitched :: BC.ByteString
--   -- glitched <- randomReplaceByte imageFile  -- glitched :: BC.ByteString
--   glitched <- randomSortSection imageFile
--   -- putStrLn (BC.unpack glitched)
--   let glitchedFileName = mconcat ["glitched_",fileName] 
--   BC.writeFile glitchedFileName glitched
--   --
--   putStrLn "Done."
--

-- main :: IO ()
-- main = do
--   args <- getArgs           -- args :: [String]
--   let fileName = head args  -- fileName :: String
--   --
--   imageFile <- BC.readFile fileName        -- imageFile :: BC.ByteString
--   glitched1 <- randomReplaceByte imageFile
--   glitched2 <- randomSortSection glitched1
--   glitched3 <- randomReplaceByte glitched2
--   glitched4 <- randomSortSection glitched3
--   glitched5 <- randomReplaceByte glitched4
--   let glitchedFileName = mconcat ["glitched_",fileName] 
--   BC.writeFile glitchedFileName glitched5
--   --
--   putStrLn "Done."
--

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [ randomReplaceByte 
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  --
  imageFile <- BC.readFile fileName
  -- glitched <- foldM (\bytes f -> f bytes) imageFile [ randomReplaceByte 
  --                                                   , randomSortSection
  --                                                   , randomReplaceByte
  --                                                   , randomSortSection
  --                                                   , randomReplaceByte
  --                                                   ]
  glitched <- foldM (\bytes f -> f bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_",fileName]
  BC.writeFile glitchedFileName glitched
  putStrLn "Done."
--