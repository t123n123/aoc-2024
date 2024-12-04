main = do
  input <- lines <$> readFile "input"
  print $ partOne input
  print $ partTwo input

check "XMAS" = 1
check "SAMX" = 1
check _ = 0

leftright :: [String] -> Int
leftright xs = check start
 where
  start = (take 4) $ head xs

updown :: [String] -> Int
updown xs = check start
 where
  start = map head (take 4 xs)

diagonal :: [String] -> Int
diagonal xs = check start
 where
  start = map (\i -> head $ (drop i) $ head $ (drop i) $ xs) [0, 1, 2, 3]

weird_diagonal :: [String] -> Int
weird_diagonal xs = check start
 where
  start = map (\i -> last $ (take (4 - i)) $ head $ (drop i) $ xs) [0, 1, 2, 3]

combine xs
  | length xs > 3 && length (head xs) > 3 = leftright xs + updown xs + diagonal xs + weird_diagonal xs
  | length xs > 3 = updown xs
  | length (head xs) > 3 = leftright xs
  | otherwise = 0

partOne :: [String] -> Int
partOne xs = sum [combine (map (drop y) (drop x xs)) | x <- [0 .. length xs - 1], y <- [0 .. length (head xs) - 1]]

check' :: [String] -> Int
check' [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = 1
check' [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = 1
check' [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = 1
check' [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = 1
check' _ = 0

partTwo xs = sum [check' . (map (take 3)) . (take 3) . (map (drop y)) . (drop x) $ xs | x <- [0 .. length xs - 3], y <- [0 .. length (head xs) - 3]]
