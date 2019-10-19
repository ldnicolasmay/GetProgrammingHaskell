-- unit4_lesson24_file_counts_strict.hs

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type FileCounts = (Int,Int,Int)

-- getCounts :: String -> FileCounts
-- getCounts fileStr = (charCount,wordCount,lineCount)
--   where charCount = length fileStr
--         wordCount = length $ words fileStr
--         lineCount = length $ lines fileStr
getCounts :: T.Text -> FileCounts
getCounts fileText = (charCount,wordCount,lineCount)
  where charCount = T.length fileText
        wordCount = length $ T.words fileText
        lineCount = length $ T.lines fileText
-- 

-- countsText :: FileCounts -> String
-- countsText (cc,wc,lc) = 
--   unwords [ "chars: ",show cc,"  " -- unwords / mconcat
--           , "words: ",show wc,"  "
--           , "lines: ",show lc ]
countsText :: FileCounts -> T.Text
countsText (cc,wc,lc) = 
  T.pack $ mconcat [ show cc,"," -- unwords / mconcat
                   , show wc,","
                   , show lc ]
--

main :: IO ()
main = do
  args        <- getArgs    -- args :: [String]
  let fileName = head args  -- fileName :: String
  --
  -- fileStr       <- readFile fileName   -- fileStr :: String
  -- let fileCounts = getCounts fileStr   -- fileCounts :: FileCounts
  -- let summaryStr = mconcat [fileName,",",countsText fileCounts,"\n"]
  fileText       <- TIO.readFile fileName  -- fileText :: T.Text
  let fileCounts  = getCounts fileText     -- fileCounts :: FileCounts
  let summaryText = mconcat [T.pack fileName,",",countsText fileCounts,"\n"]
  --
  -- appendFile "file_stats.csv" summaryStr
  TIO.appendFile "file_stats_strict.csv" summaryText
  --
  -- putStrLn summaryStr
  TIO.putStrLn summaryText
  putStrLn "Done"