module Main

import System.File
import Data.Strings
import Data.List
import Data.List.Views

solve_a : Integer -> List Integer -> Maybe Integer
solve_a target ledger with (split ledger)
  solve_a target [] | SplitNil = Nothing
  solve_a target [x] | (SplitOne x) = Nothing
  solve_a target (x :: (xs ++ (y :: ys))) | (SplitPair x xs y ys) =
    case (reverse (y :: ys)) of
      [] => Nothing
      (z :: zs) => let sum = x + z in
        case sum > target of
          True => solve_a target (x :: (xs ++ (reverse zs)))
          False => case sum < target of
                     True => solve_a target (xs ++ (reverse (z :: zs)))
                     False => Just (x * z)

solve_b : Integer -> List Integer -> List Integer -> Maybe Integer
solve_b target [] _ = Nothing
solve_b target (x :: xs) ys = case x < target of
  True => case (solve_a (target - x) (ys ++ xs)) of
    Nothing => solve_b target xs (x :: ys)
    (Just val) => Just (x * val)
  False => Nothing

main : IO ()
main = do
  res <- readFile "1.input"
  case res of
    Left e => putStrLn $ "error: " ++ (show e)
    Right r => let vals = the (List Integer) (cast <$> (words r)) in do
      putStrLn $ "answer a: " ++ show (solve_a 2020 (sort vals))
      putStrLn $ "answer b: " ++ show (solve_b 2020 (sort vals) [])
