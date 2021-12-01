import System.IO

readInput:: IO [Char]
readInput = readFile "1.input"

main :: IO ()
main = readInput >>= print
