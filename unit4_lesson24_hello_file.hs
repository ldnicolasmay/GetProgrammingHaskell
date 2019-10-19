-- unit4_lesson24_hello_file.hs

import System.IO


-- 24.1 Opening and Closing Files


-- λ> :t openFile 
-- openFile :: FilePath -> IOMode -> IO Handle

-- λ> :info FilePath
-- type FilePath = String  -- Defined in ‘GHC.IO’

-- λ> :info IOMode
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
--         -- Defined in ‘GHC.IO.IOMode’
-- instance Eq IOMode -- Defined in ‘GHC.IO.IOMode’
-- instance Ord IOMode -- Defined in ‘GHC.IO.IOMode’
-- instance Show IOMode -- Defined in ‘GHC.IO.IOMode’
-- instance Read IOMode -- Defined in ‘GHC.IO.IOMode’
-- instance Enum IOMode -- Defined in ‘GHC.IO.IOMode’

-- openFile "stuff.txt" ReadMode

-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   hClose helloFile
--   putStrLn "Done!"

-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   firstLine <- hGetLine helloFile
--   putStrLn firstLine
--   secondLine <- hGetLine helloFile
--   goodbyeFile <- openFile "goodbye.txt" WriteMode
--   hPutStrLn goodbyeFile secondLine
--   hClose helloFile
--   hClose goodbyeFile
--   putStrLn "done!"
-- --

-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   hasLine   <- hIsEOF helloFile
--   firstLine <- if not hasLine
--                then hGetLine helloFile
--                else return "empty"
--   -- putStrLn firstLine
--   putStrLn "done!"
-- --

main :: IO ()
main = do
  --
  putStrLn "Opening files..."
  helloFile   <- openFile "hello.txt" ReadMode
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  -- 
  putStrLn "Processing line 1..."
  eofLine1  <- hIsEOF helloFile
  firstLine <- if not eofLine1 
               then hGetLine helloFile 
               else return "[empty]"
  hPutStrLn goodbyeFile firstLine
  --
  putStrLn "Processing line 2..."
  eofLine2   <- hIsEOF helloFile
  secondLine <- if not eofLine2 
                then hGetLine helloFile 
                else return "[empty]"
  hPutStrLn goodbyeFile secondLine
  --
  putStrLn "Closing files..."
  hClose helloFile
  hClose goodbyeFile
  --
  putStrLn "Done."
--


