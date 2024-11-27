module Main

import System.File
import Data.Strings

solve_item_a : String -> Maybe Bool

solve_a : List String -> List (Maybe Bool)
solve_a [] = []
solve_a (x :: xs) = (solve_item_a x) :: (solve_a xs)

main : IO ()
main = do
  res <- readFile "2.input"
  case res of
    Left e => putStrLn $ "error: " ++ (show e)
    Right r => putStrLn $ show (solve_a (lines r))
