import System.IO

readInput :: IO [Char]
readInput = readFile "1.input"

getDepths :: [Char] -> [Int]
getDepths input = read <$> (lines input)

countIncrements :: [Int] -> Int
countIncrements (x:xs) = fst $ foldr (\y (c, prev) -> (if y <= prev then c + 1 else c, y)) (0, x) xs

main :: IO ()
main = do
  input <- readInput
  print $ (countIncrements . getDepths) input
