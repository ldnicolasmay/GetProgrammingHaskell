-- unit5_lesson30_echo.hs

-- echo :: IO ()
-- echo = getLine >>= putStrLn
-- -- getLine :: IO String
-- -- putStrLn :: String -> IO ()

-- main :: IO ()
-- main = echo

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and we'll echo it: " >>
              getLine >>= putStrLn

main :: IO ()
main = echoVerbose

