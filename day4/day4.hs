import Data.List (tails, transpose)

main = do
  input <- lines <$> readFile "input"
  print $ partOne input
  print $ partTwo input

check "XMAS" = 1
check "SAMX" = 1
check _ = 0

leftright :: [String] -> Int
leftright (('X' : 'M' : 'A' : 'S' : _) : _) = 1
leftright (('S' : 'A' : 'M' : 'X' : _) : _) = 1
leftright _ = 0

updown :: [String] -> Int
updown (('X' : _) : ('M' : _) : ('A' : _) : ('S' : _) : _) = 1
updown (('S' : _) : ('A' : _) : ('M' : _) : ('X' : _) : _) = 1
updown _ = 0

diagonal :: [String] -> Int
diagonal (('X' : _) : (_ : 'M' : _) : (_ : _ : 'A' : _) : (_ : _ : _ : 'S' : _) : _) = 1
diagonal (('S' : _) : (_ : 'A' : _) : (_ : _ : 'M' : _) : (_ : _ : _ : 'X' : _) : _) = 1
diagonal _ = 0

weird_diagonal :: [String] -> Int
weird_diagonal ((_ : _ : _ : 'X' : _) : (_ : _ : 'M' : _) : (_ : 'A' : _) : ('S' : _) : _) = 1
weird_diagonal ((_ : _ : _ : 'S' : _) : (_ : _ : 'A' : _) : (_ : 'M' : _) : ('X' : _) : _) = 1
weird_diagonal _ = 0

combine xs = leftright xs + updown xs + diagonal xs + weird_diagonal xs

partOne :: [String] -> Int
partOne xs = sum [sum [combine . transpose $ y | y <- tails . transpose $ x] | x <- tails xs]

check' :: [String] -> Int
check' [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = 1
check' [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = 1
check' [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = 1
check' [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = 1
check' _ = 0

partTwo xs = sum [check' . (map (take 3)) . (take 3) . (map (drop y)) . (drop x) $ xs | x <- [0 .. length xs - 3], y <- [0 .. length (head xs) - 3]]
