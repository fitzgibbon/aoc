import System.Directory
import System.File

getInput : Nat -> Nat -> IO (Maybe String)
getInput year day = do
         Right input <- readFile $ "../input/" ++ (show year) ++ "/" ++ (show day) ++ ".input"
         | Left e => do
                putStrLn $ "Error fetching input for year" ++ (show year) ++ " day " ++ (show day)
                pure Nothing
         putStrLn input
         pure $ Just input

main : IO ()
main = do
     putStrLn "Hello AoC!"
     dir <- currentDir
     putStrLn $ show dir
     input <- getInput 2023 1
     putStrLn $ show input
