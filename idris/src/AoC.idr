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
