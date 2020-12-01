module Main

import System.File
import Data.Strings
import Data.List
import Data.List.Views

solve : List Integer -> Maybe Integer
solve ledger with (split ledger)
  solve [] | SplitNil = Nothing
  solve [x] | (SplitOne x) = Nothing
  solve (x :: (xs ++ (y :: ys))) | (SplitPair x xs y ys) =
    case (reverse (y :: ys)) of
      [] => Nothing
      (z :: zs) => let target = x + z in
        case target > 2020 of
          True => solve (x :: (xs ++ (reverse zs)))
          False => case target < 2020 of
                     True => solve (xs ++ (reverse (z :: zs)))
                     False => Just (x * z)

main : IO ()
main = do
  res <- readFile "1.input"
  case res of
    Left e => putStrLn $ "error: " ++ (show e)
    Right r => let vals = the (List Integer) (cast <$> (words r)) in
                 putStrLn $ "answer: " ++ show (solve (sort vals))
