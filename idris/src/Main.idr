import System.Directory
import System.File
import Data.String

import Collie

import AoC

%hide Data.Record.SmartConstructors.infix.(::=)

aoc : Command "aoc"
aoc = basic "Advent of Code: Idris" (lotsOf nat)

{nm : String} -> {cmd : Command nm} -> Show (ParseTreeT f g cmd) where
  show (Here x) = "\{nm} <<args>>"
  show (There pos parsedSub) = "\{nm} \{show parsedSub}"

main : IO ()
main = do
     putStrLn "Hello AoC!"
     dir <- currentDir
     putStrLn $ show dir
     Right cmdParse <- aoc.parseArgs
     | Left err => putStrLn "Error: \{err}"
     input <- getInput 2023 1
     putStrLn "Parsed as: \{show cmdParse}"
