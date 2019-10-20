-- unit4_lesson24_unix_cp.hs

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs              -- args :: [String]
  let oldFileName = args !! 0  -- oldFileName :: String
  let newFileName = args !! 1  -- newFileName :: String
  --
  putStrLn $ mconcat ["Copying ",oldFileName," to ",newFileName]
  oldFileText <- TIO.readFile oldFileName  -- olfFileText :: T.Text
  TIO.writeFile newFileName oldFileText
  --
  putStrLn "Done."
--