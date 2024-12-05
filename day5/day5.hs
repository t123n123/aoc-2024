import Data.List (elemIndex, find, sortBy)

main = do
  input <- lines <$> readFile "input"
  (rel, ls) <- mySplit "" <$> pure input
  orders <- map (\rel -> both readInt $ mySplit '|' rel) <$> pure rel
  prints <- map (\s -> map readInt $ words $ (replace ',' ' ' s)) <$> pure ls
  print . sum . map middle . filter (\s -> and . map (\p -> check s p) $ orders) $ prints
  print . sum . map middle . map (fixOrder orders) . filter (\s -> or . map (\p -> not $ check s p) $ orders) $ prints

fixOrder :: [(Int, Int)] -> [Int] -> [Int]
fixOrder orders xs = sortBy getOrder xs
 where
  getOrder x y
    | find (== (x, y)) orders == Just (x, y) = LT
    | find (== (y, x)) orders == Just (y, x) = GT
    | otherwise = EQ

check :: [Int] -> (Int, Int) -> Bool
check xs (a, b) = case (elemIndex a xs, elemIndex b xs) of
  (Just a', Just b') -> a' < b'
  _ -> True

middle :: [Int] -> Int
middle xs = (!!) xs (length xs `div` 2)

replace f r [] = []
replace f r (x : xs)
  | f == x = r : rest
  | otherwise = x : rest
 where
  rest = replace f r xs

readInt :: String -> Int = read

both f (a, b) = (f a, f b)

mySplit x xs = splitAt' x xs []
 where
  splitAt' x [] ys = (reverse ys, [])
  splitAt' x (x' : xs) ys
    | (x == x') = (reverse ys, xs)
    | otherwise = splitAt' x xs (x' : ys)

consecutivePairs (x : y : xs) = (x, y) : consecutivePairs (y : xs)
consecutivePairs _ = []
