-- fileStats.hs


import System.Environment
import System.IO

type FileCounts = (Int,Int,Int)

getCounts :: String -> FileCounts
getCounts fileStr = (charCount,wordCount,lineCount)
  where charCount = length fileStr
        wordCount = length $ words fileStr
        lineCount = length $ lines fileStr
--

countsText :: FileCounts -> String
countsText (cc,wc,lc) = mconcat [show cc,",",show wc,",",show lc]

main :: IO ()
main = do
  args        <- getArgs            -- args :: [String]
  let fileName = head args          -- fileName :: String
  --
  fileStr        <- readFile fileName  -- fileStr :: String
  let fileCounts  = getCounts fileStr  -- fileCounts :: FileCounts
  let summaryText = mconcat [fileName,",",countsText fileCounts,"\n"]
  --
  appendFile "file_stats.csv" summaryText
  --
  putStrLn summaryText
  putStrLn "Done"