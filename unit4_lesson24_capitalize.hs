-- unit4_lesson24_capitalize.hs

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  args <- getArgs           -- args :: [String]
  let fileName = head args  -- fileName :: String
  --
  fileText <- TIO.readFile fileName
  let fileTextUpper = T.toUpper fileText
  TIO.writeFile fileName fileTextUpper
  --
  putStrLn "Done."