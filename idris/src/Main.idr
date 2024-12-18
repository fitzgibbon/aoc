import System.Directory
import System.File
import Data.String

import Collie

import AoC

%hide Data.Record.SmartConstructors.infix.(::=)

aoc : Command "aoc"
aoc = MkCommand 
    { description = "Advent of Code"
    , subcommands = [ "--help" ::= basic "Show usage." none
                    , "run" ::= runCmd
                    ]
    , modifiers = []
    , arguments = none
    } where
      runCmd : Command "run"
      runCmd = MkCommand
             { description = "Run a problem"
             , subcommands = []
             , modifiers = []
             , arguments = lotsOf nat
             }

handle : Main.aoc ~~> IO ()
handle = [ const $ putStrLn aoc.usage
         , "--help" ::= [ const $ putStrLn aoc.usage ]
         , "run" ::= [ \args => case args.arguments of
                                     Just n => putStrLn "\{show n}"
                                     Nothing => putStrLn "No arguments." ]
         ]

{nm : String} -> {cmd : Command nm} -> Show (ParseTreeT f g cmd) where
  show (Here x) = "\{nm} <args>"
  show (There pos parsedSub) = "\{nm} \{show parsedSub}"

main : IO ()
main = aoc.handleWith handle
