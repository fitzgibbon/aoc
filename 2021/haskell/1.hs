import Data.List

readInput :: IO [Char]
readInput = readFile "1.input"

getDepths :: [Char] -> [Int]
getDepths = (map read) . lines

countInc :: [Int] -> Int
countInc [] = 0
countInc (x:xs) = fst $ foldl' (\(c, p) y -> (c + if y > p then 1 else 0, y)) (0, x) xs

windowed :: Int -> [a] -> [[a]]
windowed width ls = if width > length ls then [] else (take width ls) : windowed width (drop 1 ls)

main :: IO ()
main = do
  input <- readInput
  print $ (countInc . getDepths) input
  print $ (countInc . (map sum) . (windowed 3) . getDepths) input
