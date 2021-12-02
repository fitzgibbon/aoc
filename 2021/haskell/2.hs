readInput :: IO [Char]
readInput = readFile "2.input"

data Dir = Forward | Up | Down deriving (Show)
data Command = Command Dir Int deriving (Show)

instance Read Dir where
  readsPrec _ input = case (lex input) of
    [("forward", r)] -> [(Forward, r)]
    [("up", r)] -> [(Up, r)]
    [("down", r)] ->[(Down, r)]
    _ -> []

instance Read Command where
  readsPrec p input = do
    (cmd, s_i) <- readsPrec p input
    (i, rest) <- readsPrec p s_i
    return (Command cmd i, rest)

getCommands :: [Char] -> [Command]
getCommands = (map read) . lines

getOffset :: Command -> Offset
getOffset (Command dir x) = case dir of
  Forward -> Offset x 0
  Up -> Offset 0 (-x)
  Down -> Offset 0 x

data Offset = Offset Int Int deriving (Show)

instance Semigroup Offset where
  Offset horizontal_a depth_a <> Offset horizontal_b depth_b = Offset (horizontal_a + horizontal_b) (depth_a + depth_b)

instance Monoid Offset where
  mempty = Offset 0 0

getMult :: Offset -> Int
getMult (Offset horizontal depth) = horizontal * depth

main :: IO ()
main = do
  input <- readInput
  print $ getMult $ mconcat $ getOffset <$> (getCommands input)
