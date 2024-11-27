readInput :: IO [Char]
readInput = readFile "2.input"

data Dir = Forward | Up | Down
data Command = Command Dir Int

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
getCommands = map read . lines

data Offset = Offset Int Int

getOffset :: Command -> Offset
getOffset (Command dir x) = case dir of
  Forward -> Offset x 0
  Up -> Offset 0 (-x)
  Down -> Offset 0 x

instance Semigroup Offset where
  Offset h_a d_a <> Offset h_b d_b = Offset (h_a + h_b) (d_a + d_b)

instance Monoid Offset where
  mempty = Offset 0 0

getMult :: Offset -> Int
getMult (Offset h d) = h * d

data Aimed = Aimed Int Offset

instance Semigroup Aimed where
  Aimed a_a (Offset h_a d_a) <> Aimed a_b (Offset h_b d_b) = Aimed (a_a + a_b) (Offset (h_a + h_b) (d_a + d_b + a_a * h_b))

instance Monoid Aimed where
  mempty = Aimed 0 mempty

getAimed :: Command -> Aimed
getAimed (Command dir x) = case dir of
  Forward -> Aimed 0 (Offset x 0)
  Up -> Aimed (-x) (Offset 0 0)
  Down -> Aimed x (Offset 0 0)

getMult' :: Aimed -> Int
getMult' (Aimed _ off) = getMult off

main :: IO ()
main = do
  input <- readInput
  print $ getMult . mconcat . map getOffset . getCommands $ input
  print $ getMult' . mconcat . map getAimed . getCommands $ input
