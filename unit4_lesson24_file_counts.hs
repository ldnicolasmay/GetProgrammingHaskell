-- unit4_lesson24_file_counts.hs


import System.IO
import System.Environment


-- 24.2 Simple I/O Tools

-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()


type FileCounts = (Int,Int,Int)


main :: IO ()
main = do
  args <- getArgs                               -- args :: [String]
  let fileName = head args                      -- fileName :: String
  helloStr <- readFile fileName                 -- helloStr :: String
  let helloCounts = getCounts helloStr          -- helloCounts :: (Int,Int,Int)
  --
  statsFile <- openFile "stats.dat" AppendMode  -- statsFile :: IO Handle
  hPutStrLn statsFile (mconcat [fileName," -- ",countsText helloCounts])
  hClose statsFile
  --
  putStrLn (mconcat [fileName," -- ",countsText helloCounts])
  putStrLn "Done"
--

-- getCounts :: String -> (Int,Int,Int)
getCounts :: String -> FileCounts
getCounts fileStr = (charCount,wordCount,lineCount)
  where charCount = length fileStr
        wordCount = length $ words fileStr
        lineCount = length $ lines fileStr
-- 

countsText :: FileCounts -> String
countsText (cc,wc,lc) = unwords [ "chars: ",show cc,"  " -- unwords / mconcat
                                , "words: ",show wc,"  "
                                , "lines: ",show lc ]
--


