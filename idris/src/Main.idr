import System.Directory
import System.File

import Collie

import AoC

main : IO ()
main = do
     putStrLn "Hello AoC!"
     dir <- currentDir
     putStrLn $ show dir
     input <- getInput 2023 1
     putStrLn $ show input
