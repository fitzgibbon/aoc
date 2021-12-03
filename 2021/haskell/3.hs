import Data.Char
import Data.Bits
import Data.List
import Data.Bool
import Numeric

readInput :: IO [Char]
readInput = readFile "3.input"

getReadings :: [Char] -> [Int]
getReadings = map (fst . head . readInt 2 (`elem` "01") digitToInt) . lines

getBits :: Int -> [Bool]
getBits = unfoldr (\i -> if i == 0 then Nothing else Just (i `testBit` 0, i `shiftR` 1))

padBits :: [[Bool]] -> [[Bool]]
padBits xs = map (\x -> x ++ (take (l - length x) (repeat False))) xs where
  l = maximum . map length $ xs

collateBits :: [[Bool]] -> [Bool]
collateBits = map ((> 0) . sum . map (bool (-1) 1)) . transpose

evalBits :: [Bool] -> Int
evalBits = foldr (\x acc -> (acc `shiftL` 1) .|. (bool 0 1 x)) 0

getMult :: [Bool] -> Int
getMult bits = evalBits bits * evalBits (map not bits)

main :: IO ()
main = do
  input <- readInput
  print $ getMult . collateBits . padBits . map getBits . getReadings $ input
