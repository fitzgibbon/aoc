module Day1

import System.File

readInput : IO (Maybe String)
readInput = do
          Right input <- readFile "/home/niall/src/personal/aoc/2023/input/Day1.input"
          | Left e => do
            putStrLn "error"
            pure Nothing
          putStrLn input
          pure $ Just input

main : IO ()
main = do
     input <- readInput
     putStrLn $ show input
