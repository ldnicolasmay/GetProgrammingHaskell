-- unit5_lesson30_ask_name.hs

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " <> name <> "!"

helloName :: IO ()
helloName = askForName >>                               -- askForName :: IO ()
            getLine >>=                                 -- getLine    :: IO String
            (\name -> return (nameStatement name)) >>=  -- ...        :: IO String
            putStrLn                                    -- putStrLn   :: String -> IO ()

main :: IO ()
main = helloName

